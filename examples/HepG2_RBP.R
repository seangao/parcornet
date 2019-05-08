library(SparkR)
library(parcornet)

sparkR.session()

df <- readNetworkBedFile('examples/data/HepG2_RBP.bed')

sparkR.stop()
