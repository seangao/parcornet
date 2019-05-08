library(dplyr)
library(SparkR)

readNetworkBedFile <- function(
  filename,
  rowIndex = 2,
  columnIndex = 1,
  valuesIndex = 3
) {
  df <- read.df(filename,
                header = 'false',
                delimiter = '\t',
                source = 'csv')

  rowCol <- paste('_c', rowIndex - 1, sep='')
  colCol <- paste('_c', columnIndex - 1, sep='')
  valCol <- paste('_c', valuesIndex - 1, sep='')

  # df <- df %>%
  #   groupBy(rowCol) %>%
  #   pivot(colCol) %>%
  #   agg(avg(valCol))

  df <- df %>%
    groupBy('_c1') %>%
    pivot('_c0') %>%
    agg(avg(column('_c2')))

  df <- fillna(df, 0)
  colnames(df)[1] <- 'TF'

  return(df)
}
