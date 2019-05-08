calcWeightMatrix <- function(x, nNodes, labels, directed=TRUE,...)
{
  # replace labels with nodes
  if (is.factor(x[,1])) x[,1] <- as.character(x[,1])
  if (is.factor(x[,2])) x[,2] <- as.character(x[,2])

  if (is.character(x[,1]) & is.character(x[,2]))
  {
    x[,1] <- match(x[,1], labels)
    x[,2] <- match(x[,2], labels)
  }

  nNodes <- max(x[,1:2])

  from <- c(x[,1], x[!directed,2])
  to <- c(x[,2] , x[!directed,1])
  if (ncol(x) == 2)
  {
    w <- rep(1,length(from))
  } else
  {
    w <- c(x[,3], x[!directed,3])
  }

  # unweighted edge list
  if ( ncol(x)==2 )
  {
    mat <- as.matrix(1*sparseMatrix(from,to, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  } else
  {
    mat <- as.matrix(1*sparseMatrix(from,to,x=w, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  }
}
