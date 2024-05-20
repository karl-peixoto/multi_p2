

#Construindo data frame
v <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10')
f1 <- c(0.803, 0.651, 0.476, 0.484, 0.218, -0.017, -0.026, 0.282, 0.436, 0.72)
f2 <- c(0.035, 0.387, 0.517, 0.55, -0.069, 0.264, 0.8, 0.715, 0.411, 0.139)
f3 <- c(0.227, -0.016, -0.059,-0.059, 0.716, 0.745, 0.257, 0.146, -0.159, 0.266)
df <- data.frame(v, f1, f2, f3)

# Calculando as comunalidades e especificidades de cada variável
df$comunalidade <- round(df$f1**2 + df$f2^2 + df$f3^2, 3)
df$especificidade <- 1 - df$comunalidade

#Calculando explicacao da variancia de cada fator
sum(df$f1 ** 2) + sum(df$f2 ** 2) + sum(df$f3 ** 2)  #fator 1 
sum(df$f2 ** 2) #fator 2

sum(df$f3 ** 2) #fator 3

sum(df$comunalidade ** 2) #total

round(0.846 ** 2 + -0.113 **2, 3)
