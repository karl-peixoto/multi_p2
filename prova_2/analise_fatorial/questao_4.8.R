
###PACOTES
pkgs = installed.packages()
if (!("readxl" %in% pkgs)){
  install.packages("readxl")
}
library('readxl')
if (!("psych" %in% pkgs)){
  install.packages("psych")
}
library('psych')
if (!("tidyverse" %in% pkgs)){
  install.packages("tidyverse")
}
library('tidyverse')

setwd("C:/Users/kmenezes/Desktop/OneDrive - unb.br/7 semestre/analise_multivariada/prova_2/")

df <- read_excel('../dados_livro/Dados de Exercícios/Stress/Stress.xlsx')


#Calaculando matriz de correlações policloricas removendo variável identificadora
cor <- polychoric(df %>% select(-c(1)) %>% as.matrix(), correct = FALSE)$rho
#Realizar analise fatorial
factanal(cor, factors = 5, )

#Construir gráfico dos autovalores (scree plot???)
#Selecionar o numero de componentes tal q eles expliquem 70% da variância


#Calcular comunalidades para o número de fatores selecionado
#Verificar se o número de fatores selecionado é adequado

#Interpretar os valores










