# SCRIPT DOS DADOS DA DISSERTA��O 
# MARINA LAPORTE COTIAS 

## Diret�rio 

setwd("C:/Users/marin/OneDrive/�rea de Trabalho/marina_teste")

## Carregar arquivo em excel 

install.packages("readxl")
library(readxl)

bd_1 <-  read_excel("teste_painel.xlsx", 1)

## teste para ver se a vari�ncia da distribui��o � igual a m�dia 

### Delegacias/N�cleos 

teste_mean_1 <- mean(bd_1$DEAM_N�cleo)
  
teste_var_1 <- var(bd_1$DEAM_N�cleo) 

### A m�dia e a vari�ncia n�o s�o iguais. 

### Desvio padr�o

sd(bd_1$DEAM_N�cleo) ###  O resultado indica um alto valor do desvio padr�o em rela��o a m�dia. 
                     ###  Analisando junto � vari�ncia, podemos observar uma alta varia��o. 

## Gr�fico da distribui��o: Deam/N�cleo

library(ggplot2)

ggplot(bd_1, aes(DEAM_N�cleo)) +
  geom_histogram(bins = 10) 

### Por se tratar de uma distribui��o muito dispersa, ou seja, 
### a varia��o condicional excede a m�dia condiciona, o mdelhor 
### modelo que se adequa � o modelo NegBin.

### Juizados/varas

teste_mean_2 <- mean(bd_1$Juizados_Varas)

teste_var_2 <- var(bd_1$Juizados_Varas)

### Desvio padr�o 

sd(bd_1$Juizados_Varas) ### 3.64 e a m�dia 3.69/ Distribui��o n�o muito dispersa

## Gr�fico 

ggplot(bd_1, aes(Juizados_Varas)) +
  geom_histogram(bins = 10) 

### Defensoria   

teste_mean_3 <- mean(bd_1$Defensoria)

teste_var_3 <- var(bd_1$Defensoria)

## Gr�fico 

ggplot(bd_1, aes(Defensoria)) +
  geom_histogram(bins = 10) 

## 
