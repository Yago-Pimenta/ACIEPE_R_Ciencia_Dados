# Aluno : Yago David Pimenta RA : 800273 

# Instalando os pacotes necessários
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("palmerpenguins")

# Lendo os pacotes necessários
library(dplyr)
library(ggplot2)
library(palmerpenguins)

# Documentação da base de dados
?penguins

# Organizando a base de dados
dados = na.omit(penguins) # Removendo os NAs (valores faltantes)
dados$year = factor(dados$year) # Transformando a variável year em fator

### Questão 1 ----
dim(dados) 

### Questão 2 ----
str(dados)

min_peso <- min(dados$body_mass_g)
media_peso <- mean(dados$body_mass_g)
max_peso <- max(dados$body_mass_g)


cat("Mínimo:", min_peso, "\n")
cat("Média:", media_peso, "\n")
cat("Máximo:", max_peso, "\n")

### Questão 3 ----



# letra a 
dados1 <- dados %>%
  rename(especies = species,
         ilha = island,
         c = bill_length_mm,
         b = bill_depth_mm,
         a = flipper_length_mm,
         peso = body_mass_g,
         sexo = sex,
         ano = year,
         )




str(dados1)

# letra b
dados2 <- dados1 %>%
  arrange(peso)




head(dados2)

# letra c
dados3 <- dados2 %>%
  filter(especies != "Gentoo")




head(dados3)


# letra d

dados_final <- dados3 %>%
  mutate(razao_comprimento_profundidade = c / b)


head(dados_final)

### Questão 4 ----


# letra a
variaveis_fator <- c("especies", "sexo")

# Loop para criar gráficos de barras para variáveis do tipo fator
for (var in variaveis_fator) {
  p <- ggplot(dados_final, aes(x = !!as.name(var))) +
    geom_bar() +
    labs(x = var, y = "Quantidade de Observações", title = paste("Gráfico de Barras para", var))
  
  print(p)
}

# letra b
install.packages("gridExtra")
library(gridExtra)
graficos <- list()
variaveis_numericas <- c("a", "b","c", "razao_comprimento_profundidade")

for (var in variaveis_numericas) {
  p <- ggplot(dados_final, aes(x = !!as.name(var))) +
    geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
    labs(x = var, y = "Quantidade de Observações", title = paste("Histograma para", var))
  
  graficos[[var]] <- p
}


grid.arrange(grobs = graficos, ncol = 2)

### Questão 5 ----


# letra a
for (var in variaveis_fator) {
  p <- ggplot(dados_final, aes(x = !!as.name(var), y = peso)) +
    geom_boxplot(fill = "lightblue") +
    labs(x = var, y = "Peso dos Pinguins", title = paste("Boxplot de", var, "em relação ao Peso"))
  
  print(p)
}

# letra b
scatter_plots <- list()

for (var in variaveis_numericas) {
  p <- ggplot(dados_final, aes(x = !!as.name(var), y = peso)) +
    geom_point(color = "blue") +
    labs(x = var, y = "Peso dos Pinguins", title = paste("Gráfico de Dispersão de", var, "em relação ao Peso"))
  
  scatter_plots[[var]] <- p
}


library(gridExtra)
grid.arrange(grobs = scatter_plots, ncol = 2)

# letra c
# -> Aparentemente o comprimento e profundidade do bico tem relação com o peso dos pinguins 

### Questão 6 ----


# letra a

modelo1 <- lm(peso ~ c, data = dados_final)


summary(modelo1)

modelo2 <- lm(peso ~ c + b + especies, data = dados_final)


summary(modelo2)

modelo3 <- lm(peso ~ c * razao_comprimento_profundidade, data = dados_final)


summary(modelo3)

# letra b
aic_modelo1 <- AIC(modelo1)
aic_modelo2 <- AIC(modelo2)
aic_modelo3 <- AIC(modelo3)


cat("AIC Modelo 1:", aic_modelo1, "\n")
cat("AIC Modelo 2:", aic_modelo2, "\n")
cat("AIC Modelo 3:", aic_modelo3, "\n")


# letra c
# -> Os tres modelos apresentados apresentam variaveis muito boas para justificar o peso do pinguin , pois ambos possuem p-value menor que 0.05 .

# letra d
#-> No melhor modelo obtemos : Multiple R-squared:  0.09019
