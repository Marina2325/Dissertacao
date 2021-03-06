### Disserta��o 
### Construindo o �ndice de implementa��o

options(scipen = 9999) ## n�mero sem nota��o cient�fica

## Definir diret�rio 

setwd("C:/Users/marin/OneDrive/�rea de Trabalho/Disserta��o/dados_final_modelo")

## Ler arquivo em .csv

install.packages("readxl")
library(readxl)

bd <- read_excel("bd_disserta��o.xlsx")

## Tranformar vari�veis em Z score (Como recomendado por Hair)

bd$DEAM_N�cleo_padron <- scale(bd$DEAM_N�cleo, center = TRUE, scale = TRUE)

bd$CEAM_padron <- scale(bd$CEAM, center = TRUE, scale = TRUE)

bd$Casa_Abrigo_padron <- scale(bd$Casa_Abrigo, center = TRUE, scale = TRUE) 

bd$juizados_Varas_padron <- scale(bd$Juizados_Varas, center = TRUE, scale = TRUE)

bd$Promotoria_padron <- scale(bd$Promotoria, center = TRUE, scale = TRUE)

bd$Defensoria_padron <- scale(bd$Defensoria, center = TRUE, scale = TRUE) 

## Criar um data frame com as vari�veis padronizadas 

indice <- data.frame(bd$DEAM_N�cleo_padron,bd$CEAM,bd$Casa_Abrigo_padron,bd$juizados_Varas_padron,
                     bd$Promotoria_padron, bd$Defensoria_padron)

## Criar o �ndice 

## An�lise de Compotentes Principais 

fit <- princomp(indice, cor=TRUE) ### inserindo dados brutos e extraindo CPs da matriz de correla��o

summary(fit) ### Resumo 

loadings(fit) ### Loadings dos CPs

plot(fit,type="lines") ### scree plot

fit$scores ## Componentes principais 

## Extrair e rotacionar fatores 

## Varimax 

install.packages('psych')
library(psych)

fit <- principal(indice, nfactors=6, rotate="varimax") ### Gerando fatores 

## Extrair 1 fator --> analisando o scree plot 

fit <- principal(indice, nfactors=1, rotate="varimax") ### Gerando 1 fator 

fit ## print : cargas fatoriais

fit$scores ### scores 

## Comunalidades

fit$communality

## Criar nova base de dados

indice$scores <- fit$scores ### Adicionando uma coluna dos scores em "indice"

indice$UF <- bd$UF ### Adiconando uma coluna com o nome dos estados 

indice$Ano <- bd$Ano

## Normalizar os scores da ACP

x <- as.vector(fit$scores)

normalized <- (x-min(x))/(max(x)-min(x))

indice$Indice_de_prote��o <- normalized 

## Construindo banco de dados para a an�lise

library(readxl)

## apagar algumas vari�veis 

install.packages("dplyr")
library(dplyr)

indice = indice %>% select(UF, Ano, Indice_de_prote��o)

## Merge bd e indice 

banco <- inner_join(bd, indice)

## teste regress�o 

install.packages("plm")
library(plm)

razaopop <- banco$POP/1000000

install.packages("writexl")
library(writexl)

write_xlsx(indice, "./indice.xlsx")

## Adequabilidade dos dados para � ACP

### Teste de Kaiser-Meyer-Olkin

KMO(fit) 

### Teste de Alfa de Cronbach 

alpha(indice)  

### Teste de esferecidade 

bart<-function(dat){ #dat is your raw data
  R<-cor(dat)
  p<-ncol(dat)
  n<-nrow(dat)
  chi2<- -((n-1)-((2*p)+5)/6 ) * log(det(R)) #this is the formula
  df<-(p*(p-1)/2)
  crit<-qchisq(.95,df) #critical value
  p<-pchisq(chi2,df,lower.tail=F) #pvalue
  cat("Bartlett's test of sphericity: X2(",
      df,")=",chi2,", p=",
      round(p,3),sep="" )   
}

bart(indice) ###  p-value < 0.005