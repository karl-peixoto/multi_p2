
setwd("C:/Users/kmenezes/Desktop/OneDrive - unb.br/7 semestre/analise_multivariada/prova_2/")
library(ggplot2)

#Definindo df de cargas fatoriais rotacionadas
x <- c('x1', 'x2', 'x3', 'x4', 'x5')
f1 <- c(-0.79, -0.07, 0.9, 0.92, -0.64)
f2 <- c(0.43, 0.96, -0.18, 0.22, 0.4)

df <- data.frame(x, f1, f2)

# Calculando as comunalidades e unicidades de cada variável
df$comunalidade <- round(df$f1^2 + df$f2^2, 3)
df$especificidade <- 1 - df$comunalidade

#Calculando a porcentagem de explicação da variância de cada fator

#Calculada pela soma dos quadrados das cargas fatoriais de cada fator 
#dividido pela soma dos quadrados das cargas fatoriais de todos os fatores
sum(df$f1 ** 2)/(sum(df$f1 ** 2) + sum(df$f2 ** 2)) #fator 1

sum(df$f2 ** 2)/(sum(df$f1 ** 2) + sum(df$f2 ** 2)) #fator 2


# Create the scatter plot
ggplot(df, aes(x = f1, y = f2, label = x)) +
  geom_point() +
  geom_text(vjust = 1, hjust = 1, size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Fator 1", y = "Fator 2", ) +
  xlim(-1, 1) +
  ylim(-1, 1)
ggsave("imagens/4.1_scatter_plot.pdf", width = 100, height = 100, units = "mm" )
