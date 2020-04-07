library(tibble)
library(dplyr)

# NB if updating Profile.xlsx, you will need to open and save it locally
# to avoid Excel's 'Protect user from internet file' mode.
messages <- openxlsx::read.xlsx("Profile.xlsx", sheet = 2)
rm.idx <- which(is.na(messages[,1]) & is.na(messages[,2]) & is.na(messages[,3]))
messages <- messages[-rm.idx,1:4]
names(messages) <- c("message_type", "key", "value", "type")
messages[,'type'] <- as.factor(messages[,'type'])

idx <- which(!is.na(messages[,1]))
messages[,1] <- rep(messages[idx,1], 
                    diff(c(idx, nrow(messages)+1)))
messages <- as_tibble(messages[-which(is.na(messages[,2])),])

fit_message_types <- split(messages[,2:4], messages[[1]])

usethis::use_data(fit_message_types, overwrite = TRUE)
