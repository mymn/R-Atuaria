omega
taxajuros=0.05
#fazer com tabua e comparar
#Nx/Dx
library(readxl)
AT49 <- read_excel("C:/Users/aluno/Downloads/AT49 (2).xlsx")
#Alterar o caminho para o local da planilha
print(AT49)
#AT49M = x,4
#AT49F = x 8

param_GGD=function(){
    lambda=runif(n=1,min=0.0003,max=0.0004)
    theta=runif(n=1,min=1.2,max=2.0)
    c=runif(n=1,min=0.06,max=0.07)
  return(lambda,theta,c)
}
#funcao para criar os parametros da GGD
#print(AT49[1,4]) para chamar

#lambda = 0.0003~0.0004
#theta = 1.2~2.0
#c = 0.06~0.07
#parametros sempre maiores que 0, estimados baseados no MMQ 
simnum=1000
Estim=c()
reserva=c()
#numero de simulacoes 
for(i in simnum){
  param_GGD()
  for(k in 1:omega){
    valor=0
    for(j in 1:110){
      Estim=c(Estim,1-(1-exp((-lambda/c)*(exp(c*(j-1)-1)))^theta))
      valor=valor+(1/(1+taxajuros)^(AT49[j,13]))*(Estim[AT49[k+j-1],13]/Estim[AT49[k,13]])
      if (AT49[k]+j-1==0){
        break
      }
    }
    reserva=c(reserva,valor)
  }
}
hist(reserva, xlab="Reserva", ylab="Frequencia")
for(i in AT49[1,13]:omega){
  BenetabM=c(BenetabM,((AT49[i,9]/AT49[i,10])*AT49[i,14])
BenetabF=c(BenetabF,)

for(j in 1:omega){
  for(i in idade:omega){
    beneficio=c(beneficio,estim[idade]*AT49[idade,15]*AT49[j,14])
  }
}
