
ReadFit <- function (fileName) {
  
  .NextByte <- function () readBin(con, 'raw', 1L)
  .NextInt <- function (bits = 1L, endian = 'little', ...) {
    readBin(con, 'int', 1L, bits, endian = endian, ...)
  }
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  
  seek(con, 0)
  headerSize <- .NextInt()
  if (!headerSize %in% c(12L, 14L)) {
    stop("Only 12- or 14-bit headers are currently supported. ",
         "Please file a bug report.")
  }
  protocolVersion <- .NextInt()
  profileVersion <- .NextInt(2L)
  dataSize <- .NextInt(4L)
  maxRecords <- floor(dataSize / 2L)
  
  fitBits <- vapply(rep(1, 4), .NextInt, 0L, signed = FALSE)
  fitChars <- intToUtf8(fitBits)
  
  if (paste0(fitChars) != '.FIT') {
    stop('File header invalid: bits 8-11 read ', fitChars, ', not .FIT')
  }
  
  crc <- if (headerSize > 12L) .NextInt(2L) else NULL
  #TODO validate CRC; C function from section 3.3.2 commented out in utilities.R
  
  dat <- readBin(con, 'raw', dataSize)
  
  messages <- list()
  messageFields <- list()
  messageEndian <- list()
  globalMessageNumbers <- fit_data_types$mesg_num
  nEntries <- integer(nrow(globalMessageNumbers))
  names(nEntries) <- globalMessageNumbers$value
  localToGlobal <- character(16)
  names(localToGlobal) <- as.character(0:15)
  
  devWarning <- FALSE
  
  nextByte <- 1
  while (nextByte < dataSize) {
    header <- rawToBits(dat[nextByte])
    
    if (header[5]) {
      stop("Read error: Reserved bit 4 not zero at data byte ", nextByte)
    }
    nextByte <- nextByte + 1L
    
    if (header[8]) { # MSB, named Bit 7 in Table 4-1
      # Compressed header
      localMessageType <- .BitsToInt(header[6:7])
      timeOffset <- .BitsToInt(header[1:5])
    } else {
      # Normal header
      localMessageType <- .BitsToInt(header[1:4])
      if (header[7]) { # Bit 6
        
        # Definition message
        
        byte0 <- nextByte
        if (dat[byte0]) {
          stop("First byte of definition is not zero at data byte ", byte0)
        }
        bigEndian <- as.logical(dat[byte0 + 1L])
        globalMessageNumber <- .BytesToInt(dat[byte0 + 2:3], bigEndian)
        globalMessageName <- as.character(globalMessageNumbers[globalMessageNumbers$key == globalMessageNumber, 'value'])
        localToGlobal[as.character(localMessageType)] <- globalMessageName
        nFields <- as.integer(dat[byte0 + 4L])
        
        message("Defined message type ", globalMessageNumber, " with ",
                nFields, " fields.")
        
        fieldDetails <- fit_message_types[[globalMessageName]]
        fields <- matrix(dat[byte0 + 4 + seq_len(nFields * 3L)], 3L)
        detailIndex <- match(as.integer(fields[1, ]), fieldDetails$key)
        
        baseTypeIndices <- as.integer(fields[3, ] & as.raw(0x1F)) + 1L
        if (!identical(fitBaseTypes[baseTypeIndices, 'size'],
                       as.integer(fields[2, ]))) {
          stop("Base type number does not match size at data byte ", byte0)
        }
        
        names(baseTypeIndices) <- unlist(fieldDetails[detailIndex, 'value'])
        fields <- as.data.frame(list(
          baseTypeIndex = baseTypeIndices,
          size = as.integer(fields[2, ]),
          baseType = fields[3, ],
          type = as.character(unlist(fieldDetails[detailIndex, 'type']))
          ),
          stringsAsFactors = FALSE)
        
        messages[[globalMessageName]] <- as.data.frame(
          matrix(nrow = maxRecords, ncol = nFields, dimnames = list(NULL, rownames(fields))))
        messageFields[[globalMessageName]] <- fields
        messageEndian[[globalMessageName]] <- bigEndian
        
        nextByte <- byte0 + 5L + (3 * nFields)
        
        if (header[6]) { # Bit 5
          if (!devWarning) {
            warning("Developer field handling not tested. ",
                    "Please report any bugs.")
            devWarning <- TRUE
          }
          devByte0 <- byte0 + 4L + (3 * nFields)
          nDevFields <- as.integer(dat[devByte0])
          devFields <- matrix(dat[devByte0 + 4 + seq_len(nDevFields * 3L)], 3L)
          nextByte <- nextByte + 1L + (3L * nDevFields)
        }
        
      } else {
        
        # Normal data message
        
        if (header[6]) { # Bit 5
          stop("Read error: Reserved bit 5 not zero at data byte ", nextByte)
        }
        
        messageType <- localToGlobal[as.character(localMessageType)]
        nEntries[messageType] <-  nEntries[messageType] + 1L
        entry <- nEntries[messageType]
        fields <- messageFields[[messageType]]
        dataBytes <- sum(fields[, 'size'])
        
        messages[[messageType]][entry, ] <- 
          .ReadData(dat[nextByte + seq_len(dataBytes) - 1L], fields)
        
        nextByte <- nextByte + dataBytes
        
      }
    }
    
    
    
  }
  
}

.ReadData <- function (bytes, fields) {
  nFields <- nrow(fields)
  sizes <- fields[, 'size']
  whichField <- rep(seq_len(nFields), sizes)
  vapply(seq_len(nFields), function (i) .BytesToInt(bytes[whichField == i]), 0)
}

.PrintMessage <- function (msg, fields) {
  ret <- msg
  fieldNames <- rownames(fields)
  dataTypeNames <- names(fit_data_types)
  for (i in seq_along(fields)) {
    if (fields$type[i] == 'date_time') {
      ret[, i] <- as.POSIXct(msg[, i], tz = 'UTC', origin = "1989-12-31 00:00")
    }
    if (fieldNames[i] %in% dataTypeNames) {
      values <- fit_data_types[[fieldNames[i]]]
      ret[, i] <- values$value[match(msg[, i], values$key)]
    }
  }
  if (all(c('manufacturer', 'product') %in% colnames(ret)) && ret[, 'manufacturer'] == 'garmin') {
    values <- fit_data_types$garmin_product
    ret[, 'product'] <- values$value[match(msg[, 'product'], values$key)]
  }
  
  ret
}