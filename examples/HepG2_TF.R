library(SparkR)
library(parcornet)

sparkR.session()

df <- readNetworkBedFile('examples/data/HepG2_TF.bed')

sparkR.stop()
