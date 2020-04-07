
## Reads the fit file header.  This should now be 14 bytes (but may be 12).
## Currently only data_size is really used, to determine how large the file is.
## 
.readFileHeader <- function(con) {
  
  header <- list()
  header$size <- readBin(con = con, what = "int", n = 1, size = 1)
  header$protocol_version <- readBin(con = con, what = "int", n = 1, size = 1)
  header$profile_version <- readBin(con = con, what = "int", n = 1, size = 2, endian = "little")
  header$data_size <- readBin(con = con, what = "int", n = 1, size = 4, endian = "little")
  header$data_type <- rawToChar(readBin(con = con, what = "raw", n = 4, size = 1))
  if(header$size == 14) {
    header$crc <- readBin(con = con, what = "int", n = 2, size = 1)
  }
  return(header)
}

#' @param con Connection to a file
#' @param last_normal_header Last uncompressed header with local message
#' type = 0, which will contain a timestamp. Compressed headers record
#' an offset against this timestamp.
#' 
## reads the 1 byte header that preceeds each record/message
## The 8th bit determines if this is a standard or compressed header and 
## we dispatch the appropriate function here
## 
.readRecordHeader <- function(con) {
  
  record_header <- rawToBits(readBin(con = con, what = "raw", n = 1, size = 1))
  if(record_header[8]) {
    header <- .readMessageHeader.compressed(record_header)
  } else {
    ## normal header
    header <- .readMessageHeader.normal(record_header)
  }
  
  dput(header)
  header
  
}

## takes the 8 bits from a normal message header
## determines if this is a definition or data message, the presence of
## developer data, and the local message type.
##
.readMessageHeader.normal <- function(record_header) {
  
  header <- list()
  header$type <- "normal"
  if (record_header[5] != 0) {
    stop("Reserved bit 4 not zero in normal header at file byte ", 
          seek(con) - 1L, ': ', ifelse(record_header, 1, 0))
  }
  header$message_type <- ifelse(record_header[7], "definition", "data")
  header$developer_data <- as.logical(record_header[6])
  if (header$message_type == 'data' && header$developer_data) {
    stop("Reserved bit 5 not zero in normal data header at file byte ", 
          seek(con) - 1L, ': ', ifelse(record_header, 1, 0))
  }
  header$local_message_type <- .binaryToInt(record_header[1:4])
  return(header)
  
}

.readMessageHeader.compressed <- function(record_header) {
  
  header <- list()
  header$type <- "compressed_timestamp"
  header$message_type <- "data"
  header$developer_data <- FALSE
  header$local_message_type <- .binaryToInt(record_header[6:7])
  header$time_offset <- .binaryToInt(record_header[1:5])
  return(header)
  
}