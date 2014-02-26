rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

Mar13 <- read.table("toc.rowdata//210313 Lysimiter FACE (detailed).txt",
                    header = TRUE, skip = 13, na.strings = c("NA"), fill = TRUE)

head(Mar13)
