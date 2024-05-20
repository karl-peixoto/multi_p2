#### Pacotes e diretório ####

rm(list=ls())
pkgs = installed.packages()

if (!("readxl" %in% pkgs)) install.packages("readxl"); 
library(readxl)
if (!("psych" %in% pkgs)) install.packages("psych"); 
library(psych)
if (!("ggplot2" %in% pkgs)) install.packages("ggplot2"); 
library(ggplot2)
if (!("TeachingDemos" %in% pkgs)) install.packages("TeachingDemos"); 
library(TeachingDemos)
if (!("aplpack" %in% pkgs)) install.packages("aplpack"); 
library(aplpack)
if (!("andrews" %in% pkgs)) install.packages("andrews"); 
library(andrews)
if (!("data.table" %in% pkgs)) install.packages("data.table"); 
library(data.table)
if (!("lattice" %in% pkgs)) install.packages("lattice"); 
library(lattice)
if (!("corrplot" %in% pkgs)) install.packages("corrplot"); 
library(corrplot)
if (!("plotly" %in% pkgs)) install.packages("plotly"); 
library(plotly)
if (!("rgl" %in% pkgs)) install.packages("rgl"); 
library(rgl)
if (!("gplots" %in% pkgs)) install.packages("gplots"); 
library(gplots)
if (!("OpenImageR" %in% pkgs)) install.packages("OpenImageR"); 
library(OpenImageR)
if (!("Matrix" %in% pkgs)) install.packages("Matrix"); 
library(Matrix)
if (!("ca" %in% pkgs)) install.packages("ca"); 
library(ca)
if (!("dplyr" %in% pkgs)) install.packages("dplyr"); 
library(dplyr)

setwd("C:/Users/jaira/Documents/UnB/5º Semestre/Multivariada/Códigos")

#### Exercício 1 - FAZEERRR ####



#### Exercício 2 ####
# Apenas interpreção dos dados



#### Exercício 3 - ERROOOO ####
dados <- read_excel("Pizza.xlsx")
dados <- dados[,c(3:9)]

corpol <- polychoric(dados) 
corp <- round(corpol$rho, 3)
corp

polychoric(matrix, correct =FALSE)


#### Exercício 4 ####
dados <- read_excel("BemEstarFin.xlsx")
corpol <- polychoric(dados) 
corp <- round(corpol$rho, 3)
corp

# Autovalores da matriz de correlações
A10 <- principal(corp, nfactors = 10, rotate = 'none')
colunas <- c("Autovalor", "% a Variância", "% Acumulada")
cbind(A10$Vaccounted[1,], 100*A10$Vaccounted[2,], 100*A10$Vaccounted[3,])

# Comunalidades, cargas fatoriais
A2 <- principal(corp, nfactors = 2, rotate = "none")
cbind(A2$communality, A2$loadings[,1], A2$loadings[,2])



#### Exercício 5 ####
dados <- read_excel("WVS6.xlsx")
M <- dados[,c(4:8)] %>% 
  as.matrix()
M

corpol <- polychoric(M) #erro!
M <- cor(M)
M

# Autovalores da matriz de correlações
A5 <- principal(M, nfactors = 5, rotate = 'none')
colunas <- c("Autovalor", "% a Variância", "% Acumulada")
cbind(A5$Vaccounted[1,], 100*A5$Vaccounted[2,], 100*A5$Vaccounted[3,])

# Comunalidades, cargas fatoriais
A2 <- principal(M, nfactors = 2, rotate = "none")
cbind(A2$communality, A2$loadings[,1], A2$loadings[,2])



#### Exercício 6 - FAZEEERR ####
dados <- read_excel("WVS6b.xlsx")
dados <- dados[,c(20:28)]

M <- cov(dados)
M



#### Exercício 7 ####
dados <- read_excel("Otolito.xlsx")
dados <- dados[,c(2:15)]

M <- cor(dados)
M

# Autovalores da matriz de correlações
A14 <- principal(M, nfactors = 14, rotate = 'none')
colunas <- c("Autovalor", "% a Variância", "% Acumulada")
cbind(A14$Vaccounted[1,], 100*A14$Vaccounted[2,], 
      100*A14$Vaccounted[3,])
# Comunalidades, cargas fatoriais - Varimax
A2 <- principal(M, nfactors = 3, rotate = "varimax")
tabela2 <- cbind(A2$communality, A2$loadings[,1], A2$loadings[,2])
round(tabela2, 3)

#Comparando com a análise de componentes principais 
cpdados <- prcomp( ~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 +
                     X9 + X10 + X11 + X12, 
                   data=dados, scale = T)
summary(cpdados) #Matriz correlações

cpdados <- prcomp( ~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 +
                     X9 + X10 + X11 + X12, 
                   data=dados, scale = F)
summary(cpdados) #Matriz covariâncias
## Verificar normalidade multivariada!!!!



#### Exercício 8 - ERROOOO ####
dados <- read_excel("Stress.xlsx")
dados <- cbind(dados$X1, dados$X2, dados$X3, dados$X4, dados$X5,
               dados$X6, dados$X7, dados$X8, dados$X9, dados$X10,
               dados$X11, dados$X12, dados$X13, dados$X14)
colnames(dados) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                     "X9", "X10", "X11", "X12", "X13", "X14")
dados

#Matriz de correlações policóricas
corpol <- polychoric(dados)



#### Exercício 9 ####
dados <- read_excel("Stress.xlsx")
dados <- cbind(dados$X1, dados$X2, dados$X3, dados$X4, dados$X5,
               dados$X6, dados$X7, dados$X8, dados$X9, dados$X10,
               dados$X11, dados$X12, dados$X13, dados$X14)
colnames(dados) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                     "X9", "X10", "X11", "X12", "X13", "X14")

# Matriz de correlação de pearson
M <- cor(dados)
head(round(M,3))

# Autovalores da matriz de correlações
A14 <- principal(M, nfactors = 14, rotate = 'none')
colunas <- c("Autovalor", "% a Variância", "% Acumulada")
cbind(A14$Vaccounted[1,], 100*A14$Vaccounted[2,], 
      100*A14$Vaccounted[3,])

# Comunalidades, cargas fatoriais
A2 <- principal(M, nfactors = 6, rotate = "none")
cbind(A2$communality, A2$loadings[,1], A2$loadings[,2])

