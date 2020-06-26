getwd()
COM270 <- read.csv(file = "G:/R/R/Comparar dos columnas/270COM.csv", header = TRUE, sep = ";", dec = ".")
COM60 <- read.csv(file = "G:/R/R/Comparar dos columnas/60COM.csv", header = TRUE, sep = ";", dec = ".")
COM270$comp <- ifelse(COM270$Cod_CPNP %in% COM60$Cod_CPNP, 1, NA)
