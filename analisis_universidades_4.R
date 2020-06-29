setwd("G:/Investigando/Jose Carlos Veliz")
donaciones <- read.csv(file="donaciones.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)
df<-data.frame(porcentaje_donaciones=donaciones$`X._alumni_donors`, deuda_estudiantes=donaciones$`Average.student.debt`, ingresos_alumni=donaciones$`Early.career.earnings`)
### Probamos la normalidad de las variables
### H0 : La distribución de la variable aleatoria no es distinta a la distribución normal
### H1 : La distribución de la variable aleatoria es distinta a la distribución normal
## porcentaje_donaciones
hist(df$porcentaje_donaciones)
x1.test <- shapiro.test(df$porcentaje_donaciones)
print(x1.test)
LnDonaciones <- log(df$porcentaje_donaciones)
x11.test <- shapiro.test(LnDonaciones)
print(x11.test)
## Luego de la transformación, el test de Shapiro Wilk arroja normalidad estadística, por lo que integramos la variable al data frame
# p-value es mayor a 0.05, por lo tanto es una distribución normal
df$Ln_donaciones <- -log(df$porcentaje_donaciones)
df2 <- data.frame(df$Ln_donaciones, df$deuda_estudiantes, df$ingresos_alumni)
## deuda_estudiantes
hist(df$deuda_estudiantes)
x2.test <- shapiro.test(df$deuda_estudiantes)
print(x2.test)
# p-value es mayor a 0.05, por lo tanto es una distribución normal
## ingresos_alumni
hist(df$ingresos_alumni)
x3.test <- shapiro.test(df$ingresos_alumni)
print(x3.test)
### analizando la correlación entre variables
corr_modelo<-cor(df2[,], method = "pearson")
corr_modelo
### No es necesario, ya que hay normalidad en 2 de tres variables corr_coeficientes1<-cor.test(x=df$deuda_estudiantes, y=df$porcentaje_donaciones)
### No es necesario, ya que hay normalidad en 2 de tres variables corr_coeficientes1
library(corrplot)
corrplot(corr_modelo[,], method = "number")
library(ggplot2)
library(tidyverse)
install.packages("GGally", repos = "http://cran.us.r-project.org")
library(GGally)
install.packages("foreign", repos = "http://cran.us.r-project.org")
library(foreign)
install.packages("apaTables", repos = "http://cran.us.r-project.org")
library(apaTables)
install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")
library(PerformanceAnalytics)
##install.packages("psych", repos = "http://cran.us.r-project.org")
##library(psych)
##install.packages("corrr", repos = "http://cran.us.r-project.org")
##library(corrr)
apa.cor.table(df2, filename = "paper_universidades.doc", table.number = 1, show.conf.interval = FALSE, landscape = TRUE)
df2 %>% ggpairs(columns = c("df.Ln_donaciones", "df.deuda_estudiantes", "df.ingresos_alumni"), upper = list(continuous = wrap('cor', size=8)))
### Probando la significancia de la correlación
## H0 : No existe correlación entre las variables
## H1 : Existe una correlación entre las variables
## LN_donaciones y deuda_estudiantes
cor.test(~ df.Ln_donaciones + df.deuda_estudiantes, data = df2, method= "pearson")
## el p-value resultó menor a 0.05, por lo tanto si hay correlación entre estas variables 
## LN_donaciones y ingresos_alumni
cor.test(~ df.Ln_donaciones + df.ingresos_alumni, data = df2, method = "pearson")
## el p-value resultó mayor a 0.05, por lo tanto no hay correlación entre estas variables
## ingresos_alumni y deuda_estudiantes
cor.test(~ df.ingresos_alumni + df.deuda_estudiantes, data = df2, method = "pearson")
## el p-value resultó menor a 0.05, por lo tanto si hay correlación entre estas variables