library(SparkR)

# nonparanormal transformation with shrunken ECDF
nonparanormalTransform <- function(x){
  n <- nrow(x)
  d <- ncol(x)
  x.col <- colnames(x)
  x.row <- rownames(x)

  x <- apply(as.data.frame(x), 2, rank)
  x <- qnorm(x / (n + 1))
  x <- x / sd(x[,1])

  colnames(x) <- x.col
  rownames(x) <- x.row
  x <- createDataFrame(as.data.frame(x))

  return(x)
}
