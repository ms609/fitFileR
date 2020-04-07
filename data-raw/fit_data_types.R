library(tibble)
library(dplyr)

# NB if updating Profile.xlsx, you will need to open and save it locally
# to avoid Excel's 'Protect user from internet file' mode.
data_types <- openxlsx::read.xlsx("Profile.xlsx", sheet = 1)[,c(1,4,3,2)]
data_types <- setNames(data_types, c("data_type", "key", "value", "key_type"))
idx <- which(!is.na(data_types[,1]))
data_types[,1] <- rep(data_types[idx,1], 
                      diff(c(idx, nrow(data_types)+1)))
data_types[,4] <- rep(data_types[idx,4], 
                      diff(c(idx, nrow(data_types)+1)))
data_types <- as_tibble(data_types[-which(is.na(data_types[,2])),])
fit_data_types <- split(data_types[,2:4], data_types[[1]])
fit_data_types <- lapply(fit_data_types, FUN = function(x) {
  mutate(x, key = ifelse(grepl('uint32', key_type), 
                         yes = as.numeric(key), 
                         no = as.integer(key))) %>%
    select(-key_type)
})

usethis::use_data(fit_data_types, overwrite = TRUE)
