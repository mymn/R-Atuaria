library(ggplot2)
library(scales)
omega=110
taxajuros=0.05
#fazer com tabua e comparar
#Nx/Dx
library(readxl)
AT49 <- read_excel("C:/Users/TEMP/Downloads/P1 - at49 (1).xlsx")
#Alterar o caminho para o local da planilha
#AT49M = x,4
#AT49F = x 8
AT49<-as.data.frame(AT49)
Estim=vector("numeric")

param_GGD=function(){
  GGD=vector("double",3)
  as.vector(GGD)
  GGD[1]<-runif(n=1,min=0.0003,max=0.0004)
  GGD[2]<-runif(n=1,min=1.2,max=2.0)
  GGD[3]<-runif(n=1,min=0.06,max=0.07)
}
#funcao para criar os parametros da GGD
#onde param[1]=theta, param[2]=lambda e param[3]=c
#parametros sempre maiores que 0, estimados baseados no MMQ 
simnum=1000
#numero de simulacoes 

survestim=function(omega,param_GGD,simnum){
  as.vector(Estim)
  param=vector("double",3)
  param_GGD() 
    param[1]=GGD[1]
    param[2]=GGD[2]
    param[3]=GGD[3]
  for(i in 1:simnum){
    for(j in 1:omega){
      Estim[j]=Estim[j]+(1-(1-exp((-param[2]/param[3])*(exp(param[3]*(j-1)-1)))^param[1]));Estim         
    }
  }
  for(i in 1:omega){
    Estim[i]=Estim[i]/simnum;Estim
  }
}
#funcao para calculo da curva estimada de sobrevivencia por monte carlo

beneficio=function(AT49,omega,survestim,param_GGD){
  bene = 0
  for(j in 1:110){
    idade = AT49[j,13]
    while(idade < omega){
      bene=bene+(Estim[i]*AT49[j,14]*AT49[i,15]);bene
      idade =idade+1
    }
  }
}
#calculo da reserva da carteira BD em 1 iteracao

repeticoes=50
somareserva=function(beneficio,survestim,repeticoes){
  reserva=c()
  for(i in 1:repeticoes){
    survestim(omega,param_GGD,simnum)
    reserva=c(reserva,bene[i])
    print(reserva[i])
  }
}

hist(reserva)
ggplot(somareserva)+scale_y_continuous(labels=percent)+labs(x='Reserva',y='Porcentagem')

#9 e 10 para M, 11 e 12 para F (Dx e Nx)
#13 para idade 14 beneficio

somaM=0 #AT49M
somaF=0 #AT49F
reservatabM=function(omega,AT49){
  for(j in 1:omega){
    idade=AT49[j,13]
    somaM=somaM+AT49[idade,9]/AT49[idade,10]*AT49[idade,14]
  }
  return(somaM)
}
reservatabF=function(omega,AT49){
  for(j in 1:omega){
    idade=AT49[j,13]
    somaF=somaF+AT49[idade,11]/AT49[idade,12]*AT49[idade,14]
  }
  return(somaF)
}


