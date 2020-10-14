# SCRIPT DOS DADOS DA DISSERTAÇÃO 
# MARINA LAPORTE COTIAS 

## Diretório 

setwd("C:/Users/marin/OneDrive/Área de Trabalho/marina_teste")

## Carregar arquivo em excel 

install.packages("readxl")
library(readxl)

bd_1 <-  read_excel("teste_painel.xlsx", 1)

## teste para ver se a variância da distribuição é igual a média 

### Delegacias/Núcleos 

teste_mean_1 <- mean(bd_1$DEAM_Núcleo)
  
teste_var_1 <- var(bd_1$DEAM_Núcleo) 

### A média e a variância não são iguais. 

### Desvio padrão

sd(bd_1$DEAM_Núcleo) ###  O resultado indica um alto valor do desvio padrão em relação a média. 
                     ###  Analisando junto à variância, podemos observar uma alta variação. 

## Gráfico da distribuição: Deam/Núcleo

library(ggplot2)

ggplot(bd_1, aes(DEAM_Núcleo)) +
  geom_histogram(bins = 10) 

### Por se tratar de uma distribuição muito dispersa, ou seja, 
### a variação condicional excede a média condiciona, o mdelhor 
### modelo que se adequa é o modelo NegBin.

### Juizados/varas

teste_mean_2 <- mean(bd_1$Juizados_Varas)

teste_var_2 <- var(bd_1$Juizados_Varas)

### Desvio padrão 

sd(bd_1$Juizados_Varas) ### 3.64 e a média 3.69/ Distribuição não muito dispersa

## Gráfico 

ggplot(bd_1, aes(Juizados_Varas)) +
  geom_histogram(bins = 10) 

### Defensoria   

teste_mean_3 <- mean(bd_1$Defensoria)

teste_var_3 <- var(bd_1$Defensoria)

## Gráfico 

ggplot(bd_1, aes(Defensoria)) +
  geom_histogram(bins = 10) 

## 
