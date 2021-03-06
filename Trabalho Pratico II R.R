#############################################################################################
# M�TODOS QUANTITATIVOS
# Mestrado em Contabilidade e Finan�as
#
# AUTOCORRELA��O COM DADOS TEMPORAIS
#
# Instituto Superior de Contabilidade e Administra��o do Porto
# Prof. Patr�cia Ramos
#############################################################################################

rm(list = ls())

load("C:/Users/Utilizador/Desktop/Metodos Quantitativos/Trabalho 2/pib.RData")
pib

#####################################################################################################################
#                                                    ALINEA A                                                        
#####################################################################################################################
# Estime um modelo AR(2) para a varia��o percentual do PIB. Explicite a equa��o do modelo estimado. 
# Relacione a ordem do modelo autoregressivo com a an�lise do gr�fico da fun��o de autocorrela��o da s�rie.


# Explicite a equa��o do modelo estimado.

# Definir a s�rie temporal (t=1,.,250) 

tsdata <- ts(pib, start = c(1947, 2), frequency = 4)
tsdata

# Definir a equa��o do modelo estimado

library(dynlm)
ar2 <- dynlm(tsdata ~ L(tsdata) + L(tsdata,2))
summary(ar2)


myH0 <- (c("L(tsdata)=0","L(tsdata, 2)=0"))
suppressMessages(library(car))
linearHypothesis(ar2, myH0)


# Relacione a ordem do modelo autoregressivo com a analise do grafico da fun��o de autocorrela��o
# da serie.
suppressMessages(library(forecast))
acf(tsdata)
Acf(tsdata)
Acf(tsdata, plot = FALSE)


#####################################################################################################################
#                                                     ALINEA B                                                       
#####################################################################################################################
# Determine o gr�fico da fun��o de autocorrela��o dos res�duos do modelo estimado em (a).
# Quais s�o as autocorrela��es estatisticamente diferentes de zero?

suppressMessages(library(forecast))
Acf(residuals(ar2), plot = FALSE)
Acf(residuals(ar2))


#####################################################################################################################
#                                                     ALINEA C                                                      
#####################################################################################################################
# Teste a aus�ncia de autocorrela��o contra a alternativa de erros AR(2) no modelo estimado em (a). O que conclui?

# Teste autom�tico:
# Teste F contra alternativa de erros AR(2): 
library(lmtest)
bgtest(ar2, order = 2)

# Uma vez que valor-p=0.5864, n�o rejeitamos H0 (aus�ncia de autocorrela��o at� � ordem 2) 
# ao n�vel de signific�ncia de 5%, o que n�o evid�ncia que n�o h� ind�cios de autocorrela��o de erros AR(2).


#####################################################################################################################
#                                                     ALINEA D                                                    
#####################################################################################################################
# Estime, agora, um modelo AR(3) para a varia��o percentual do PIB. Explicite a equa��o do modelo estimado. 
# Determine o gr�fico da fun��o de autocorrela�ao dos res�duos. Quais s�o as autocorrela��es estatisticamente diferentes de zero? 
# Teste a aus�ncia de autocorrela��o contra a alternativa de erros AR(2). O que conclui?
  
# Definir a equa��o do modelo estimado
library(dynlm)
ar3 <- dynlm(tsdata ~ L(tsdata) + L(tsdata,2)+ L(tsdata,3))
summary(ar3)


# gr�fico da fun��o de autocorrela�ao dos res�duos
suppressMessages(library(forecast))
Acf(residuals(ar3))
Acf(residuals(ar3),plot = FALSE)

# Teste autom�tico:
# Teste F contra alternativa de erros AR(2):
library(lmtest)
bgtest(ar3, order = 2)

#####################################################################################################################
#                                                      ALINEA E                                                   
#####################################################################################################################
# Utilizando os crit�rios de sele��o de modelos estudados indique qual dos modelos, AR(2) ou AR(3), � o mais adequado.

# R^2 Ajustado

summary(ar2)$adj.r.squared    # R2 ajustado do AR(2)
summary(ar3)$adj.r.squared    # R2 ajustado do AR(3)

# BIC

# Crit�rio de Schwarz para o AR(2):
k1 <- 2                                         # no. de variaveis explicativas do AR(2)
n3 <- nobs(ar2)                                 # no. de observacoes do AR(2)
SQR1 <- t(residuals(ar2)) %*% residuals(ar2)    # SQR1
BIC1 <- log(SQR1/n3) + ((k1 + 1)/n3)*log(n3)    # BIC do AR(2)
BIC1

# Crit�rio de Schwarz para o AR(3):
k2 <- 3                                         # no. de variaveis explicativas do AR(3)
n4 <- nobs(ar3)                                 # no. de observacoes do AR(3)
SQR2 <- t(residuals(ar3)) %*% residuals(ar3)    # SQR2
BIC2 <- log(SQR2/n4) + ((k2 + 1)/n4)*log(n4)    # BIC do AR(3)
BIC2

# AIC

# Crit�rio de informa��o de Akaike para o modelo AR(2):
AIC1 <- log(SQR1/n3) + 2*k1/n3    
AIC1


# Crit�rio de informa��o de Akaike para o AR(3):
AIC2 <- log(SQR2/n4) + 2*k2/n4    
AIC2

suppressMessages(library(stargazer))
stargazer(list(ar2,ar3), type = "text", keep.stat = c("adj.rsq"), add.lines=list( c("BIC", round(BIC1,3), round(BIC2,3) ), c("AIC", round(AIC1,3), round(AIC2,3)) ) )


#####################################################################################################################
#                                                      ALINEA F                                                   
#####################################################################################################################
# Utilizando o modelo estimado em (d) fa�a uma previs�o da varia��o percentual do PIB para o 4o trimestre de 2009, o 1o trimestre de 2010 e o 2o trimestre de 2010.

pib_t4_2009 <- summary(ar3)$coefficients[1,1] + summary(ar3)$coefficients[2,1] * tsdata[250] + summary(ar3)$coefficients[3,1] * tsdata[249]  + summary(ar3)$coefficients[4,1] * tsdata[248]  
pib_t4_2009  


pib_t1_2010  <- summary(ar3)$coefficients[1,1] + summary(ar3)$coefficients[2,1] * pib_t4_2009 + summary(ar3)$coefficients[3,1] * tsdata[250]  + summary(ar3)$coefficients[4,1] * tsdata[249]  
pib_t1_2010 


pib_t2_2010 <- summary(ar3)$coefficients[1,1] + summary(ar3)$coefficients[2,1] * pib_t1_2010 + summary(ar3)$coefficients[3,1] * pib_t4_2009  + summary(ar3)$coefficients[4,1] * tsdata[250]  
pib_t2_2010
