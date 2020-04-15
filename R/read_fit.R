# TODO set dud values to NA.


data("fit_data_types")
data("fit_message_types")
data("fit_global_messages")

#' Read a FIT file
#' 
#' @return `ReadFit()` returns an object of class `FitData`.
#' @template MRS
#' @export
ReadFit <- function (fileName, verbose = FALSE) {
  
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
  names(localToGlobal) <- formatC(0:15, digits = 1, flag = '0')
  
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
      stop("Compressed header at byte ", nextByte, " not yet supported")
      
    } else {
      
      # Normal header
      
      localMessageType <- .BitsToInt(header[1:4])
      
      
      if (header[7]) { # Bit 6
        
        ## Definition message
        
        byte0 <- nextByte
        if (dat[byte0]) {
          stop("First byte of definition is not zero at data byte ", byte0)
        }
        bigEndian <- as.logical(dat[byte0 + 1L])
        nFields <- as.integer(dat[byte0 + 4L])
        fields <- matrix(dat[byte0 + 4 + seq_len(nFields * 3L)], 3L)
        
        
        globalMessageNumber <- .BytesToInt(dat[byte0 + 2:3], bigEndian)
        globalMessageName <- globalMessageNumbers[globalMessageNumbers$key == globalMessageNumber, 'value']
        if (length(globalMessageName[[1]])) {
          
          globalMessageName <- as.character(globalMessageName)
          fieldDetails <- fit_message_types[[globalMessageName]]
          detailIndex <- match(as.integer(fields[1, ]), fieldDetails$key)
          fieldNames <- unlist(fieldDetails[detailIndex, 'value'])
          fieldNames[is.na(fieldNames)] <- 
            paste('Cstm fld', as.integer(fields[1, is.na(fieldNames)]))
          fieldTypes <- as.character(unlist(fieldDetails[detailIndex, 'type']))
          
        } else {
          
          globalMessageName <- .CustomMessageName(globalMessageNumber)
          nEntries[globalMessageName] <- 0
          fieldNames <- paste('Cstm fld', seq_len(nFields))
          fieldTypes <- rep(NA, nFields)
          
        }
        localToGlobal[as.character(localMessageType)] <- globalMessageName
        
        if (verbose) {
          message("\n\nBytes following ", byte0 - 1L, 
                  " assign local message type ", localMessageType, " to:\n  ",
                  globalMessageNumber, ": '", globalMessageName, 
                  "' (", nFields, " fields).")
        }
        
        baseTypeIndices <- as.integer(fields[3, ] & as.raw(0x1F)) + 1L
        btiSize <- fitBaseTypes[baseTypeIndices, 'size']
        fldSize <- as.integer(fields[2, ])
        if (!identical(btiSize, fldSize)) {
          warning("Base type number does not match size in fields [",
                  paste(fieldNames[btiSize != fldSize], collapse = ', '),
                  '].\n  Message: "', globalMessageName, '"; data byte: ', byte0)
        }
        
        names(baseTypeIndices) <- fieldNames
        fields <- as.data.frame(list(
          baseTypeIndex = baseTypeIndices,
          size = as.integer(fields[2, ]),
          baseType = fields[3, ],
          type = fieldTypes
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
        
        ## Normal data message
        
        if (header[6]) { # Bit 5
          stop("Read error: Reserved bit 5 not zero at data byte ", nextByte)
        }
        
        messageType <- localToGlobal[as.character(localMessageType)]
        nEntries[messageType] <-  nEntries[messageType] + 1L
        entry <- nEntries[messageType]
        fields <- messageFields[[messageType]]
        dataBytes <- sum(fields[, 'size'])
        
        messages[[messageType]][entry, ] <- 
          .ReadData(dat[nextByte + seq_len(dataBytes) - 1L], fields,
                    messageEndian[[messageType]])
        
        if (verbose) {
          output <- capture.output(print(.PrintMessage(messages[[messageType]][entry, ], fields)))
          for (i in seq_len(length(output) / 2)) {
            message(output[i + i])
            message(output[i + i + 1L])
          }
        }
        nextByte <- nextByte + dataBytes
        
      }
    }
    
    
  }
  
  for (msgType in names(messages)) {
    messages[[msgType]] <- messages[[msgType]][seq_len(nEntries[msgType]), ]
  }
  
  structure(list(fields = messageFields,
                 messages = messages),
            class='FitData')
}

#' @exportClass FitData
NULL

#' @export
print.FitData <- function (x, ...) {
  cat("FitData object with message types: \n ", 
      paste(names(x$messages), collapse = '\n  '))
}

#' @export
FitItem <- function (dat, i, ...) {
  messages <- dat[['messages']][[i]]
  fields <- dat[['fields']][[i]]
  .PrintMessage(messages, fields)
}
  

.CustomMessageName <- function (x) {
  paste0('custom_msg_', x)
}

.ReadData <- function (bytes, fields, bigEndian) {
  nFields <- nrow(fields)
  sizes <- fields[, 'size']
  whichField <- rep(seq_len(nFields), sizes)
  vapply(seq_len(nFields), 
         function (i) .BytesToInt(bytes[whichField == i], bigEndian),
         0)
}

.PrintMessage <- function (msg, fields) {
  ret <- msg
  fieldNames <- rownames(fields)
  dataTypeNames <- names(fit_data_types)
  
  for (i in seq_len(nrow(fields))) {
    if (fields$type[i] %in% dataTypeNames) {
      switch(fields$type[i],
             'date_time' = {
               ret[, i] <- as.POSIXct(msg[, i], tz = 'UTC', origin = "1989-12-31 00:00")
             },
             {
               values <- fit_data_types[[fields$type[i]]]
               ret[, i] <- values$value[match(msg[, i], values$key)]
             }
      )
    }
  }
     
  positions <- fieldNames %in% c('position_lat', 'position_long')
  ret[, positions] <- msg[, positions] * (180 / (2^31))
  
  manufacturer <- ret[1, 'manufacturer']
  if (all(c('manufacturer', 'product') %in% colnames(ret)) && 
      !is.na(manufacturer) && manufacturer == 'garmin') {
    # Assume that all entries relate to same device...
    values <- fit_data_types$garmin_product
    prodKey <- match(msg[, 'product'], values$key)
    ret[!is.na(prodKey), 'product'] <- values$value[prodKey[!is.na(prodKey)]]
  }
  
  ret
}