library(ggplot2)

centralityPlot <- function(..., labels, scale = c("z-scores", "raw", "raw0","relative"),
                           theme_bw = TRUE, print = TRUE,
                           verbose = TRUE, standardized, relative, weighted = TRUE, signed = TRUE,
                           orderBy = "default", # Can also be one of the measures
                           decreasing = FALSE
)
{
  scale <- match.arg(scale)
  if (!missing(standardized)){
  } else {
    standardized <- scale == "z-scores"
  }

  if (!missing(relative)){
  } else {
    relative <- scale == "relative"
  }

  # dummies
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL

  Long <- centralityTable(..., standardized=standardized, labels=labels, relative=relative, weighted = weighted, signed = signed)

  # ordering by node name
  if (orderBy == "default"){
    nodeLevels <- unique(gtools::mixedsort(as.character(Long$node), decreasing = decreasing))
  } else {
    nodeLevels <- names(sort(tapply(Long$value[Long$measure == orderBy],Long$node[Long$measure == orderBy],mean), decreasing=decreasing))
  }
  Long$node <- factor(as.character(Long$node), levels = nodeLevels)
  Long <- Long[gtools::mixedorder(Long$node),]

  # plot
  if (length(unique(Long$type)) > 1)
  {
    g <- ggplot(Long, aes(x = value, y = node, group = type, colour = type))
  } else {
    g <- ggplot(Long, aes(x = value, y = node, group = type))
  }

  g <- g +  geom_path() +  xlab("") + ylab("") + geom_point()

  if (length(unique(Long$graph)) > 1)
  {
    g <- g + facet_grid(graph ~ measure, scales = "free")
  } else
  {
    g <- g + facet_grid( ~ measure, scales = "free")
  }

  if (theme_bw){
    g <- g + theme_bw()
  }

  if (scale == "raw0"){
    g <-g + xlim(0,NA)
  }


  if (print){
    print(g)
    invisible(g)
  } else {
    return(g)
  }
}

