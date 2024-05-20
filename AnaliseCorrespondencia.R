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
if (!("factoextra" %in% pkgs)) install.packages("factoextra"); 
library(factoextra)

setwd("C:/Users/jaira/Documents/UnB/5º Semestre/Multivariada/Códigos")


#### Exercício 1 - FAZERRRR ####
dados <- Covid19SP <- read_excel("Covid19SP.xlsx")
head(dados)
dados <- dados[c("GrupoPop", "Prevalência", "Mortalidade", "Letalidade")]

#Letra A
quantile(dados$Prevalência)
quantile(dados$Mortalidade)
quantile(dados$Letalidade)

dados <- dados %>% 
  mutate(
    Prevalência = case_when(
      dados$Prevalência <= 79.64173 ~ "Classe 1",
      dados$Prevalência > 79.64173 & dados$Prevalência <= 105.51229 ~ "Classe 2",
      dados$Prevalência > 105.51229 & dados$Prevalência <= 127.83984 ~ "Classe 3",
      dados$Prevalência > 127.83984 ~ "Classe 4"),
    Mortalidade = case_when(
      dados$Mortalidade <= 21.316911 ~ "Classe 1",
      dados$Mortalidade > 21.316911 & dados$Mortalidade <= 28.059046 ~ "Classe 2",
      dados$Mortalidade > 28.059046 & dados$Mortalidade < 36.246476 ~ "Classe 3",
      dados$Mortalidade > 36.246476 ~ "Classe 4"),
    Letalidade = case_when(
      dados$Letalidade <= 2.083197 ~ "Classe 1",
      dados$Letalidade > 2.083197 & dados$Letalidade <= 2.788462 ~ "Classe 2",
      dados$Letalidade > 2.788462 & dados$Letalidade < 3.571429 ~ "Classe 3",
      dados$Letalidade > 3.571429 ~ "Classe 4"))

#Letra B
freq_table
M <- dados %>% 
  count(Mortalidade, by = GrupoPop)
M
frequency(dados$GrupoPop)

M <- matrix(c(161, 161, 161, 161, 162, 160, ))



#### Exercício 2 - ERROOO ####
dados <- read_excel("Mochila.xlsx", sheet = "dados")
dadosa <- dados[,c("TipoMochila","Escoliose")]

M <- matrix(c(8,16,2,37,35,10,35,72,11), nrow = 3)
rownames(M) <- c("Carrinho", "Escapular", "Lateral")
colnames(M) <- c("Ausente", "Direito", "Esquerdo")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Agrupando categorias ModoCarregar
dados <- read_excel("Mochila.xlsx", sheet = "dados")
dadosb <- dados[,c("ModoCarregar","Escoliose")] %>% 
  mutate(
    ModoCarregar = case_when(
      dados$ModoCarregar == 1 ~ "1",
      dados$ModoCarregar == 2 ~ "2",
      dados$ModoCarregar == 3 ~ "1",
      dados$ModoCarregar == 4 ~ "2",
      dados$ModoCarregar == 5 ~ "2",
      dados$ModoCarregar == 6 ~ "3"))

M <- matrix(c(8,16,2,37,35,10,35,72,11), nrow = 3)
rownames(M) <- c("Carrinho", "Escapular", "Lateral")
colnames(M) <- c("Ausente", "Direito", "Esquerdo")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")






#### Exercício 3 ####

#Criação da tabela 
M <- matrix(c(688, 326, 343, 98, 116,
              38, 84, 48, 584, 241,
              909, 403, 188, 110, 412,
              681, 4, 3, 26, 85), nrow = 4)
colnames(M) <- c("Loiro", "Vermelho", "Castanho claro", "Castanho Escuro",
                 "Preto")
rownames(M) <- c("Verde", "Azul", "Castanho", "Preto")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Porcentagem da inércia princial explicada pelos eixos
summary(ac)

inercia <- matrix(c(0.199245, 0.030087, 0.000859, 86.6 , 13.1, 
                    0.4), nrow = 3)
colnames(inercia) <- c("Inércia principal", "% explicada")
rownames(inercia) <- c("Eixo 1", "Eixo 2", "Eixo 3")
inercia #Matriz para melhor visualização

#Qualidade da representação de cada categoria
summary(ac)
    ##Qualidade das linhas
quallinha <- matrix(c(0.995, 0.979, 0.999, 1.000), nrow = 4)
colnames(quallinha) <- c("Qualidade")
rownames(quallinha) <- c("Verde", "Azul", "Castanho", "Preto")
quallinha #Matriz para melhor visualização
    ##Qualidade das colunas
qualcol <- matrix(c(1.000, 0.803, 1.000, 1.000, 0.998), nrow = 5)
colnames(qualcol) <- c("Qualidade")
rownames(qualcol) <- c("Loiro", "Vermelho", "Castanho", "Cas-Escuro", "Preto")
qualcol #Matriz para melhor visualização

#Cor olhos/cabelo que mais explicam as inércias dos eixos
row <- get_ca_row(ac)
row
head(row$contrib) #Tabela dos eixos
head(row$inertia) #Total da tabela

col <- get_ca_col(ac)
col
head(col$contrib) #Tabela dos eixos
head(col$inertia) #Total da tabela ERROOO!!!!

#Contribuição dos eixos para categoria olhos
head(row$cos2)

#Contribuição dos eixos para categoria cabelo
head(col$cos2)



#### Exercício 4 - ERROOOOOOO #### 
dados <- read_excel("BancoAlemao.xlsx")

ac1 <- dados[,c("X4", "X9")] %>% 
  filter(X4 != 'N/A') %>% 
  filter(X9 != 'N/A') %>% 
  as.matrix()
ac1

ac <- ca(ac1)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")



#### Exercício 5 #### 
#Matriz com os agrupamentos
M <- matrix(c(5699, 4029, 5314, 2306, 1231,
              15679, 11323, 16947, 8248, 5325,
              20338, 16160, 27373, 15436, 10304,
              7142, 5349, 9192, 5333, 3343,
              4386, 3241, 5089, 2477, 1388), nrow = 5)
rownames(M) <- c("Até 17 anos", "18 a 29 anos", "30 a 49 anos",
                 "50 a 64 anos", "65 ou mais anos")
colnames(M) <- c("Norte", "Nordeste", "Sudeste", "Sul",
                 "C. Oeste")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Porcentagem da inércia princial explicada pelos eixos
summary(ac) #Necessário 4 eixos para explicar 100% da inércia

inercia <- matrix(c(0.007695, 0.000118, 1e-050, 7e-050 , 97.6, 
                    1.5, 0.6, 0.2), nrow = 4)
colnames(inercia) <- c("Inércia principal", "% explicada")
rownames(inercia) <- c("Eixo 1", "Eixo 2", "Eixo 3", "Eixo 4")
inercia #Matriz sai com erro!!

#Qualidade da representação de cada categoria
summary(ac)
  ##Qualidade das linhas
quallinha <- matrix(c(0.994, 0.972, 0.886, 0.993, 1.000), nrow = 5)
colnames(quallinha) <- c("Qualidade")
rownames(quallinha) <- c("Até 17", "18-29", "30-49", "50-64", "65+")
quallinha #Matriz para melhor visualização
##Qualidade das colunas
qualcol <- matrix(c(0.998, 0.996, 0.995, 0.943, 0.974), nrow = 5)
colnames(qualcol) <- c("Qualidade")
rownames(qualcol) <- c("Nort", "Nrds", "Sdst", "Sul", "COst")
qualcol #Matriz para melhor visualização

#Região que mais explica a inércia dos eixos
col <- get_ca_col(ac)
col
head(col$contrib) #Tabela dos eixos
head(col$inertia) #Total da tabela ERROOO!!!!

#Contribuição dos eixos para categoria faixa etária
row <- get_ca_row(ac)
head(row$cos2)

#### Exercício 6 ####
M <- matrix(c(627,1105,1269,1119,605,561,
              2592,4475,5015,3699,1681,1054,
              4918,8301,8228,6067,2144,1082,
              1680,3089,2975,1933,686,305,
              772,1447,1409,1120,463,223), nrow = 6)
rownames(M) <- c("1","2","3","4","5","6 ou mais")
colnames(M) <- c("Norte", "Nordeste", "Sudeste", "Sul", "C. Oeste")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Porcentagem da inércia princial explicada pelos eixos
summary(ac) #Necessários 4 eixos para explicar 100% da inércia

inercia <- matrix(c(0.014117, 0.000274, 0.000255, 0.000135 , 95.5, 
                    1.9, 1.7, 0.9), nrow = 4)
colnames(inercia) <- c("Inércia principal", "% explicada")
rownames(inercia) <- c("Eixo 1", "Eixo 2", "Eixo 3", "Eixo 4")
inercia #Matriz para melhor visualização

#Qualidade da representação de cada categoria
summary(ac)
##Qualidade das linhas (Moradores)
quallinha <- matrix(c(0.935, 0.957, 0.639, 0.614, 0.997, 
                      1.000), nrow = 6)
colnames(quallinha) <- c("Qualidade")
rownames(quallinha) <- c("1","2","3","4","5","6 ou mais")
quallinha #Matriz para melhor visualização
##Qualidade das colunas (Região)
qualcol <- matrix(c(0.999, 0.969, 0.954, 0.935, 0.405), nrow = 5)
colnames(qualcol) <- c("Qualidade")
rownames(qualcol) <- c("Norte", "Nordeste", "Sudeste", "Sul", "C. Oeste")
qualcol #Matriz para melhor visualização

#Região que mais explica a inércia dos eixos
col <- get_ca_col(ac)
col
head(col$contrib) #Tabela dos eixos
head(col$inertia) #Total da tabela ERROOO!!!!

#Contribuição dos eixos para categoria faixa etária
row <- get_ca_row(ac)
head(row$cos2)



#### Exercício 7 ####
M <- matrix(c(3279, 14190, 45438, 22813, 5746,
              1388, 6572, 8580, 1321, 1437,
              13640, 36323, 34642, 6041, 9256), nrow = 5)
rownames(M) <- c("Norte", "Nordeste", "Sudeste", "Sul",
                 "Centro-Oeste")
colnames(M) <- c("Branca", "Preta", "Parda")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Porcentagem da inércia princial explicada pelos eixos
summary(ac) #Necessários 2 eixos para explicar 100% da inércia

inercia <- matrix(c(0.136717, 0.003416, 97.6, 2.4), nrow = 2)
colnames(inercia) <- c("Inércia principal", "% explicada")
rownames(inercia) <- c("Eixo 1", "Eixo 2")
inercia #Matriz para melhor visualização

#Qualidade da representação de cada categoria
summary(ac) ##Todas as qualidades são iguais a 1.000

#Região que mais explica a inércia dos eixos
row <- get_ca_row(ac)
row
head(row$contrib) #Tabela dos eixos
head(row$inertia) #Total da tabela ERROOO!!!!

#Contribuição dos eixos para categoria raça
col <- get_ca_col(ac)
head(col$cos2)


#### Exercício 8 - FAZERRRR ####
M1 <- matrix(c(3279, 14190, 45438, 22813, 5746,
              1388, 6572, 8580, 1321, 1437,
              13640, 36323, 34642, 6041, 9256), nrow = 5)
rownames(M1) <- c("Norte", "Nordeste", "Sudeste", "Sul",
                 "Centro-Oeste")
colnames(M1) <- c("Branca", "Preta", "Parda")
M1
M2 <- matrix(c(5699, 4029, 5314, 2306, 1231,
              15679, 11323, 16947, 8248, 5325,
              20338, 16160, 27373, 15436, 10304,
              7142, 5349, 9192, 5333, 3343,
              4386, 3241, 5089, 2477, 1388), nrow = 5)
rownames(M2) <- c("Até 17 anos", "18 a 29 anos", "30 a 49 anos",
                 "50 a 64 anos", "65 ou mais anos")
colnames(M2) <- c("Norte", "Nordeste", "Sudeste", "Sul",
                 "C. Oeste")
M2

M <- rbind(t(M1),M2)

ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#### Exercício 9 ####
M <- matrix(c(125, 56, 91, 16,
              356, 108, 240, 55,
              155, 124, 259, 76,
              53, 60, 103, 28,
              69, 59, 79, 18), nrow = 4)
rownames(M) <- c("Fund. I.", "Médio I.", "Superior I.",
                 "Superior C.")
colnames(M) <- c("Norte", "Nordeste", "Sudeste",
                 "Sul", "C. Oeste")
M

#Análise de correspondência
ac <- ca(M)
ac
summary(ac)

plot(ac) #Mapa simétrico
plot(ac, map = "rowprincipal") #Mapa assimétrico
plot(ac, map = "colprincipal")

#Porcentagem da inércia princial explicada pelos eixos
summary(ac) #Necessários 3 eixos para explicar 100% da inércia
inercia <- matrix(c(0.048834, 0.005416, 5e-050, 89.9, 10.0, 0.1), nrow = 3)
colnames(inercia) <- c("Inércia principal", "% explicada")
rownames(inercia) <- c("Eixo 1", "Eixo 2", "Eixo 3")
inercia #Matriz para melhor visualização

#Qualidade da representação de cada categoria
summary(ac) 

#Região que mais explica a inércia dos eixos
col <- get_ca_col(ac)
col
head(col$contrib) #Tabela dos eixos
head(col$inertia) #Total da tabela ERROOO!!!!

#Contribuição dos eixos para categoria grau de instrução
row <- get_ca_row(ac)
head(row$cos2)
