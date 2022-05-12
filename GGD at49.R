library(ggplot2)
library(scales)
omega=110
taxajuros=0.05
#fazer com tabua e comparar
#Nx/Dx
library(readxl)
AT49 <- read_excel("D:/BrowserDownloads/P1 - at49.xlsx")
#Alterar o caminho para o local da planilha
#AT49M = x,4
#AT49F = x 8


param_GGD=function(){
  param=c()
  param[1]=runif(n=simnum,min=0.0003,max=0.0004)
  param[2]=runif(n=simnum,min=1.2,max=2.0)
  param[3]=runif(n=simnum,min=0.06,max=0.07)
  return(param)
}
#funcao para criar os parametros da GGD
#onde param[1]=theta, param[2]=lambda e param[3]=c
#parametros sempre maiores que 0, estimados baseados no MMQ 
simnum=1000
#numero de simulacoes 



survestim=function(omega,param_GGD,simnum){
  Estim=c()
  param_GGD()
  for(j in j:omega){
    Estim[j]=Estim[j]+(1-(1-exp((-param[2]/param[3])*(exp(param[3]*(j-1)-1)))^param[1]))             
  }
  for(i in 1:omega){
    Estim[i]=Estim[i]/simnum
  }
return(Estim)
}
#funcao para calculo da curva estimada de sobrevivencia por monte carlo

beneficio=function(AT49,omega,survestim,param_GGD){
  bene = 0
  for(j in 1:110){
    i = AT49[j,13]
    while(i < omega){
      bene=bene+(Estim[i]*AT49[j,14]*AT49[i,15])
      i =i+1
    }
  }
  return(bene)
}
#calculo da reserva da carteira BD em 1 iteracao

repeticoes=50
somareserva=function(beneficio,survestim){
  reserva=c()
  for(i in 1:repeticoes){
    survestim()
    reserva=c(reserva,beneficio)
  }
  return(reserva)
}
ggplot(reserva)+scale_y_continuous(labels=percent)+labs(x='Reserva',y='Porcentagem')

