# Instalando e carregando o pacote readODS 

install.packages("readODS")
library(readODS)

# Lendo o arquivo .ods 

dados <- read_ods("Regressão Linear.ods",
                 sheet = 1,
                 skip = 1)

# Visualizando os dados

dados
head(dados)
library(tidyverse)
glimpse (dados)

# Modificando o nome das variáveis

names(dados)
dados <- dados |>
  rename(temperatura = 'Temperatura (°C)',
         venda_sorvete = 'Venda de sorvetes (em mil reais)') |>
  select(temperatura, venda_sorvete)

# Visualizando a relação entre as variáveis

library(ggplot2)

ggplot(
  data = dados,
  mapping = aes(x = temperatura, y = venda_sorvete)
) +
  geom_point()

ggplot(
  data = dados,
  mapping = aes(x = temperatura, y = venda_sorvete)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Estimando o modelo de regressão simples

modelo <- lm(venda_sorvete ~ temperatura, data = dados)

summary(modelo)

summary(m)$r.squared     
  #obs (R^2): quão bem o modelo explica os dados (quanto da variabilidade dos dados é explicada)
summary(m)$adj.r.squared 
  #obs (Adjusted R^2): Mesmo, penalizando pela quantidade de variáveis
AIC(modelo)                   
  #obs (AIC): Quão bom o modelo será em novas predições; Bom para comparar modelo; Penaliza pelo número de parâmetros
BIC(modelo)                   
  #obs (BIC): Maior penalidade pelo n. de parâmetros

# Soma dos erros quadrados
seq <- sum((predict(modelo) - dados$venda_sorvete)^2)

# Raiz do erro quadrado médio
rseq <- sqrt(mean((predict(modelo) - dados$venda_sorvete)^2))
