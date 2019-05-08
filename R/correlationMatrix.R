library(SparkR)

correlationMatrix <- function(df) {
  converted <- as.data.frame(df[, -c(1)])
  mat <- cor(converted)
  return(mat)
}
