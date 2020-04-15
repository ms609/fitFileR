# Table 4-6 FIT Base Types and Invalid Values

fitBaseTypes <- data.frame(list(
  type = 0:16,
  endianAble = as.logical(c(0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1)),
  baseTypeField = as.raw(c(0x00:0x02, 0x83:0x86, 0x07, 0x88, 0x89, 0x0A, 0x8B,
                           0x8C, 0x0D, 0x8E:0x90)),
  typeName = c('enum', 'sint8', 'uint8', 'sint16', 'uint16', 'sint32', 'uint32',
               'string', 'float32', 'float64', 'uint8z', 'uint16z', 'utin32z',
               'byte', 'sint64', 'uint64', 'uint64z'),
  size = c(1L, 1L, 1L, 2L, 2L, 4L, 4L, 1L, 4L, 8L, 1L, 2L, 4L, 1L, 8L, 8L, 8L)
))
                          
  
usethis::use_data(fitBaseTypes, internal = TRUE, overwrite = TRUE)
