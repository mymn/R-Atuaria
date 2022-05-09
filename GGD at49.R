omega
taxajuros
#fazer com tabua e comparar
#Nx/Dx
library(readxl)
AT49 <- read_excel("C:/Users/aluno/Downloads/AT49 (2).xlsx")
#Alterar o caminho para o local da planilha
print(AT49)
#AT49M = x,4
#AT49F = x 8

#print(AT49[1,4]) para chamar

lambda = 0.0003~0.0004
theta = 1.2~2.0
c = 0.06~0.07
#parametros sempre maiores que 0

sobreestim=function(lambda,c,theta){
  for(i in 1:110){
    Estim[i]=1-(1-EXP((-lambda/c)*(EXP(c*(i-1)-1)))^theta
  }
return(Estim)
}

beneficio=function(Estim,taxajuros){
  valor=0
  for(i in ...){
    valor=valor+(1/(1+taxajuros)^(i-1))*Estim(i)
  }
  return(valor)
}

reserva=function(Estim,omega,taxajuros){
  somatotal=0
  for(i in 1:...){
    somatotal=somatotal+beneficio(Estim,taxajuros,i)
  }
}

histogramas=function(reserva...){
  for(i in ...){
    ggplot...
  }
}