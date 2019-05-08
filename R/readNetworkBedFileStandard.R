library(reshape2)

readNetworkBedFileStandard <- function(filename) {
  bed <- read.csv(filename,
                 header = FALSE,
                 sep = '\t')
  df <- acast(bed, V2~V1, value.var='V3')
  df[is.na(df)] <- 0
  return(df)
}
