centrality <- function(graph,alpha=1,posfun=abs,pkg = c("igraph","qgraph"),all.shortest.paths=FALSE,
                       weighted = TRUE, signed = TRUE)
{

  W <- calcWeightMatrix(graph)
  if (!isTRUE(weighted)){
    W <- sign(W)
  }

  if (!isTRUE(signed)){
    W <- abs(W)
  }

  pkg <- match.arg(pkg)
  if (is.list(W))
  {
    return(lapply(W,centrality, alpha=alpha,posfun=posfun))
  }

  n <- nrow(W)

  # Remove diagonal:
  if (any(diag(W)!=0))
  {
    diag(W) <- 0
  }

  ## Compute adjacency:
  X <- 1L * (W!=0)

  ## Compute default measures:
  UnweightedDegreesOut <- rowSums(X)
  WeightedDegreesOut <- rowSums(posfun(W))
  CombinedDegreesOut <- UnweightedDegreesOut^(1-alpha) * WeightedDegreesOut^alpha

  UnweightedDegreesIn <- colSums(X)
  WeightedDegreesIn <- colSums(posfun(W))
  CombinedDegreesIn <- UnweightedDegreesIn^(1-alpha) * WeightedDegreesIn^alpha

  # Expected Influence
  InExpectedInfluence <- colSums(W)
  OutExpectedInfluence <- rowSums(W)

  DistMat <- 1/(ifelse(posfun(W)==0,0,posfun(W)^alpha))
  if (pkg=="igraph"){
    igraphObject <- igraph::graph.adjacency(DistMat, weighted = TRUE, mode = "directed")

    Closeness <- igraph::closeness(igraphObject)

    E <- cbind(c(row(W)),c(col(W)),c(posfun(W)))

    igraphObject <- igraph::graph_from_edgelist(E[,1:2, drop=FALSE],directed=TRUE)



    E(igraphObject)$weight <- 1/E[,3]
    igraphObject <- igraph::delete_edges(igraphObject, which(E(igraphObject)$weight == Inf))

    Betweenness <-  igraph::estimate_betweenness(igraphObject,cutoff = 1/1e-10)

    ShortestPaths <- igraph::shortest.paths(igraphObject, mode = "out")


    ls <- vector("list",n^2)
    Paths <- structure( ls, .Dim = c(n, n))

    if (all.shortest.paths){
      for (i in 1:n)
      {
        allPaths <- lapply(igraph::all_shortest_paths(igraphObject,i,V(igraphObject))$res,as.numeric)
        last <- sapply(allPaths,function(x)x[length(x)])

        for (j in 1:n)
        {
          if (i==j){
            Paths[[i,j]] <- list()
          } else {
            Paths[[i,j]] <-  allPaths[last==j]
          }
        }
      }
    }

  } else {
    # Compute shortest distance using Dijkstra (code based on pseudo code on Wikipedia)
    # Setup:

    ShortestPaths <- matrix(Inf,n,n)
    ls <- list()
    for (i in 1:n^2) ls[[i]] <- numeric(0)
    Previous <- structure(ls, .Dim = c(n, n))

    # Main loop:
    for (source in 1:n)
    {
      dist <- rep(Inf,n)
      dist[source] <- 0                     # Distance from source to source
      Q <- 1:n                              # All nodes in the graph are unoptimized - thus are in Q
      while (length(Q) > 0)                 # The main loop
      {
        u <- Q[which.min(dist[Q])]
        if (dist[u] == Inf)  break          # all remaining vertices are inaccessible from source
        Q <- Q[- which(Q==u)]
        for (v in Q)                        # where v has not yet been removed from Q.
        {
          alt <- dist[u] + DistMat[u,v]
          if (alt < dist[v])                # Relax (u,v,a)
          {
            dist[v] <- alt
            Previous[[source,v]] <- which(dist + DistMat[,v] == alt)
          }
        }
      }
      ShortestPaths[source,] <- dist
    }

    # Compute Closeness:
    Closeness <- 1/rowSums(ShortestPaths)

    # Betweenness dummy:
    Betweenness <- numeric(n)

    Gtot <- apply(Paths,1:2,sapply,length)

    # Compute betweenness:
    for (i in 1:n)
    {
      G <- apply(Paths,1:2,sapply,function(x)sum(i==unlist(x)))
      Grat <- G[-i,-i]/Gtot[-i,-i]
      Betweenness[i] <- sum(Grat[!is.nan(Grat)])
    }
  }

  lab <- function(x,labs){
    if (is.vector(x)){
      names(x) <- labs
    } else {
      rownames(x) <- colnames(x) <- labs
    }
    return(x)
  }
  Labels <- colnames(W)

  retval <- list(
    Closeness = lab(Closeness,Labels),
    Betweenness = lab(Betweenness,Labels),
    InExpectedInfluence = InExpectedInfluence)

  return(retval)
}
