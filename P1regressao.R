library(readxl)
MLR<-read_excel("D:/BrowserDownloads/Avaliação_P01_bd.xlsx") #importacao dos dados
MLR<-as.data.frame(MLR)

#dummy
library(fastDummies)
MLR=dummy_cols(MLR) #criar dummies
MLR=MLR[,-c(3,4)] #remover colunas

MLR[,12]=0   #dummy referencia
MLR[,15]=0  #dummy referencia

model2=lm(MLR) #regressao linear
summary(model2) #dados da regressao
MLR=MLR[,-c(10)] #deletar a coluna especificada (com o maior pvalor e > que 0.05)
#rodar a regressao novamente até todos p-valores serem menores que 0.05 (Stepwise "manual")

questao1=2860.98-63.17*30+(-141.17)*5+(-89.31)*6+712.14+679.12 #gabarito seria 1115.55
questao2=2860.98-712.14*2+(-63.17)*35+(-141.17)*4+(-89.31)*6+679.12*3-(2860.98-712.14*2+(-63.17)*35+(-141.17)*4+(-89.31)*6+679.12)
questao3=("o modelo passa em todos os testes")
questao4=("a contribuicao media dos numeros de dependentes eh maior que a que a do tempo de emprego, sendo isso observado pela comparacao dos coeficientes em questao 679.12 > -141.17")
questao5=("como o coeficiente da divida do cartao eh positivo, a reducao dessa divida ira causar uma reducao no valor medio das perdas")
questao6=-63.17 #coeficiente da idade
MLR
#Shapiro
shapiro.test(model2$residuals)
#Kolmogorov
ks.test(model2$residuals,"pnorm",mean(model2$residuals),sd(model2$residuals))
#white
library(skedastic)
white_lm(model2)
