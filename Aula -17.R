#Aula 17 - Modelando_Volatilidades

remove.packages("readxl")
install.packages("readxl", dependencies = T)
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)
remove.packages("tseries")
install.packages("tseries", dependencies = T)
install.packages("rugarch")

library("rugarch")
library("readxl")
library("aTSA")
library("tseries")
library("urca") 
library("forecast")

BITCOIN <- na.omit(read_excel("C:/Econometria/Bitcoin.xls"))

Bitcoin <-  ts(log(BITCOIN$Close), start = 2014, frequency = 365)


#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(log(BITCOIN$Close))
IntegradaOrdem1 <- ts(IntOrdem1, start = 2014, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança dos Logs do Bitcoin - LogReturn", ylab="Log Preço", xlab="Data", col="Pink")
grid(col = "black", lty = "dotted")


#Estimando Regressões e Tabelando Resultados
est1 <- data.frame()
for (i in 1:21) {                 #Loop para os AR: ARIMA(i,0,0)
  est1[i,1] <- paste("AR",i)      #Coluna com os nomes do Modelo
  est1[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores AIC
  est1[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores BIC
}
Resultados <- data.frame(rbind(est1))  
colnames(Resultados) <- c("Modelo","AIC","BIC")

#Estimando Regressões e Tabelando Resultados
AR1 <- arima(IntegradaOrdem1, order = c(1,1,0))   #Estima o AR1 e salva os resultados como AR1
AR2 <- arima(IntegradaOrdem1, order = c(2,1,0))   #Estima o AR2 e salva os resultados como AR2
AR3 <- arima(IntegradaOrdem1, order = c(3,1,0))   #Estima o AR3 e salva os resultados como AR3
AR4 <- arima(IntegradaOrdem1, order = c(4,1,0))   #Estima o AR4 e salva os resultados como AR4
AR5 <- arima(IntegradaOrdem1, order = c(5,1,0))   #Estima o AR5 e salva os resultados como AR5
AR6 <- arima(IntegradaOrdem1, order = c(6,1,0))   #Estima o AR6 e salva os resultados como AR6
AR7 <- arima(IntegradaOrdem1, order = c(7,1,0))   #Estima o AR7 e salva os resultados como AR7
AR8 <- arima(IntegradaOrdem1, order = c(8,1,0))   #Estima o AR8 e salva os resultados como AR8
AR9 <- arima(IntegradaOrdem1, order = c(9,1,0))   #Estima o AR9 e salva os resultados como AR9
AR10 <- arima(IntegradaOrdem1, order = c(10,1,0))   #Estima o AR10 e salva os resultados como AR10
AR11 <- arima(IntegradaOrdem1, order = c(11,1,0))   #Estima o AR11 e salva os resultados como AR11
AR12 <- arima(IntegradaOrdem1, order = c(12,1,0))   #Estima o AR12 e salva os resultados como AR12
AR13 <- arima(IntegradaOrdem1, order = c(13,1,0))   #Estima o AR13 e salva os resultados como AR13
AR14 <- arima(IntegradaOrdem1, order = c(14,1,0))   #Estima o AR14 e salva os resultados como AR14
AR15 <- arima(IntegradaOrdem1, order = c(15,1,0))   #Estima o AR15 e salva os resultados como AR15
AR16 <- arima(IntegradaOrdem1, order = c(16,1,0))   #Estima o AR16 e salva os resultados como AR16
AR17 <- arima(IntegradaOrdem1, order = c(17,1,0))   #Estima o AR17 e salva os resultados como AR17
AR18 <- arima(IntegradaOrdem1, order = c(18,1,0))   #Estima o AR18 e salva os resultados como AR18
AR19 <- arima(IntegradaOrdem1, order = c(19,1,0))   #Estima o AR19 e salva os resultados como AR19
AR20 <- arima(IntegradaOrdem1, order = c(20,1,0))   #Estima o AR20 e salva os resultados como AR20
AR21 <- arima(IntegradaOrdem1, order = c(21,1,0))   #Estima o AR21 e salva os resultados como AR21


estimacoes <- list(AR1, AR2,AR3,AR4,AR5,
                   AR6,AR7,AR8,AR9,AR10,
                   AR11,AR12,AR13,AR14,AR15,
                   AR16,AR17,AR18,AR19,AR20,AR21)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

AIC <- sapply(estimacoes, AIC)      #Cria Coluna com resultados AIC
BIC <- sapply(estimacoes, BIC)      #Cria Coluna com resultados BIC
Modelo <- c("AR1", "AR2","AR3","AR4","AR5",
            "AR6","AR7","AR8","AR9","AR10",
            "AR11","AR12","AR13","AR14","AR15",
            "AR16","AR17","AR18","AR19","AR20","AR21")   #cria coluna com nome dos modelos

Resultados <- data.frame(Modelo, AIC, BIC)  #Junta as três colunas acima num único resultado
View(Resultados)

#Efetuar teste ARCH-LM para o melhor modelo

arch.test(AR1)

#Modelando a Variância

residuos <- AR1$residuals
plot(residuos, type="o", main="Residuos do AR1")
grid(col = "black", lty = "dotted")

#FAC  e FACP  dos Residuos

acf(residuos,lend=2, lwd=5,col="Pink",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(residuos,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

GARCH202 <- garch(IntegradaOrdem1,c(20,2),trace=F)
GARCH201 <- garch(IntegradaOrdem1,c(20,1),trace=F)
GARCH192 <- garch(IntegradaOrdem1,c(19,2),trace=F)
GARCH191 <- garch(IntegradaOrdem1,c(19,1),trace=F)

GARCH182 <- garch(IntegradaOrdem1,c(18,2),trace=F) 
GARCH181 <- garch(IntegradaOrdem1,c(18,1),trace=F)

GARCH172 <- garch(IntegradaOrdem1,c(17,2),trace=F)
GARCH171 <- garch(IntegradaOrdem1,c(17,1),trace=F)

GARCH162 <- garch(IntegradaOrdem1,c(16,2),trace=F)
GARCH161 <- garch(IntegradaOrdem1,c(16,1),trace=F)

GARCH152 <- garch(IntegradaOrdem1,c(15,2),trace=F)
GARCH151 <- garch(IntegradaOrdem1,c(15,1),trace=F)

GARCH142 <- garch(IntegradaOrdem1,c(14,2),trace=F)
GARCH141 <- garch(IntegradaOrdem1,c(14,1),trace=F)

GARCH132 <- garch(IntegradaOrdem1,c(13,2),trace=F)
GARCH131 <- garch(IntegradaOrdem1,c(13,1),trace=F)

GARCH122 <- garch(IntegradaOrdem1,c(12,2),trace=F)
GARCH121 <- garch(IntegradaOrdem1,c(12,1),trace=F)

GARCH112 <- garch(IntegradaOrdem1,c(11,2),trace=F)
GARCH111 <- garch(IntegradaOrdem1,c(11,1),trace=F)
GARCH102 <- garch(IntegradaOrdem1,c(10,2),trace=F)
GARCH101 <- garch(IntegradaOrdem1,c(10,1),trace=F)
GARCH92 <- garch(IntegradaOrdem1,c(9,2),trace=F)
GARCH91 <- garch(IntegradaOrdem1,c(9,1),trace=F)

GARCH82 <- garch(IntegradaOrdem1,c(8,2),trace=F)
GARCH81 <- garch(IntegradaOrdem1,c(8,1),trace=F)
GARCH72 <- garch(IntegradaOrdem1,c(7,2),trace=F)
GARCH71 <- garch(IntegradaOrdem1,c(7,1),trace=F)

GARCH62 <- garch(IntegradaOrdem1,c(6,2),trace=F)
GARCH61 <- garch(IntegradaOrdem1,c(6,1),trace=F)
GARCH52 <- garch(IntegradaOrdem1,c(5,2),trace=F)
GARCH51 <- garch(IntegradaOrdem1,c(5,1),trace=F)

GARCH42 <- garch(IntegradaOrdem1,c(4,2),trace=F)
GARCH41 <- garch(IntegradaOrdem1,c(4,1),trace=F)

GARCH22 <- garch(IntegradaOrdem1,c(2,2),trace=F)
GARCH21 <- garch(IntegradaOrdem1,c(2,1),trace=F)
GARCH11 <- garch(IntegradaOrdem1,c(1,1),trace=F)
GARCH12 <- garch(IntegradaOrdem1,c(4,1),trace=F)

estimacoes_garch <- list(GARCH202, GARCH201,GARCH192,GARCH182,GARCH181,
                         GARCH172,GARCH171,GARCH162,GARCH161,GARCH152,GARCH151,GARCH142,
                         GARCH141,GARCH132,GARCH131,GARCH122,GARCH121,GARCH112,GARCH111,
                         GARCH102,GARCH101,GARCH92,GARCH91,GARCH82,GARCH81,GARCH72,GARCH71,
                         GARCH62,GARCH61,GARCH52,GARCH51,GARCH42,GARCH41,GARCH22,GARCH21,GARCH12,GARCH11)

AIC_Garch <- sapply(estimacoes_garch, AIC)      #Cria Coluna com resultados AIC

Modelos_Garch <- c("GARCH202", "GARCH201","GARCH1902","GARCH182","GARCH181","GARCH172",
                   "GARCH171","GARCH162","GARCH161","GARCH152","GARCH151","GARCH142","GARCH141",
                   "GARCH132","GARCH131","GARCH122","GARCH121","GARCH112","GARCH111",
                   "GARCH102","GARCH101","GARCH92","GARCH91","GARCH82","GARCH81","GARCH72","GARCH71",
                   "GARCH61","GARCH52","GARCH51","GARCH41",
                   "GARCH62","GARCH61","GARCH52","GARCH51","GARCH42","GARCH41","GARCH22","GARCH21","GARCH12","GARCH11")#cria coluna com nome dos modelos

Resultados_garch <- data.frame(Modelos_Garch, AIC_Garch)  #Junta as três colunas acima num único resultado
View(Resultados_garch)

previsao1 <- predict(GARCH03,IntegradaOrdem1)

plot(previsao1,type="o", main="Volatilidade do Bitcoin", ylab="Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")

previsao2 <- predict(GARCH71,IntegradaOrdem1,15)
plot(previsao2,type="o", main="Volatilidade do Bitcoin", ylab="Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")