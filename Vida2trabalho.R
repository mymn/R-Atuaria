#Titular X, Conjuge Y, Dependente Z
#1 dataframe para tabuas
#3 dataframes para 2 vidas
#7 dataframes para 3 vidas
#colnames(Dataframe) <- c("Idade", "qx", "l", "d", "v^x", "Dx", "N", "Cx", "M")
#nomenclatura com 'r' devido a conflito com funcoes nativas do R
#colunas impares das tabuas F, pares M. 
library(svDialogs)
input=function(){ #funcao com os inputs
  IdadeTitular<<-as.numeric(dlgInput("Idade do titular ", Sys.info()["user"])$res)
  IdadeConjuge<<-as.numeric(dlgInput("Idade do conjuge ", Sys.info()["user"])$res)
  IdadeDependente<<-as.numeric(dlgInput("Idade do dependente (0 a 24) ", Sys.info()["user"])$res)
  SexoTitular<<-as.character(dlgInput("Sexo do titular (M ou F) ", Sys.info()["user"])$res)
  SexoConjuge<<-as.character(dlgInput("Sexo do conjuge (M ou F) ", Sys.info()["user"])$res)
  SexoDependente<<-as.character(dlgInput("Sexo do dependente (M ou F) ", Sys.info()["user"])$res)
  taxajuros<<-as.numeric(dlgInput("Taxa de juros (decimal) ", Sys.info()["user"])$res)
  TipoTabua<<-as.numeric(dlgInput("Tabua(1=AT2000,2=B...,3=...): ", Sys.info()["user"])$res)
  nvidas<<-as.numeric(dlgInput("Numero de vidas(2 ou 3): ", Sys.info()["user"])$res)
  pensaomorte<<-as.numeric(dlgInput("Pensao por morte(sim=1, nao=0): ", Sys.info()["user"])$res)
  valorfacebene<<-as.numeric(dlgInput("Valor de face do beneficio: ", Sys.info()["user"])$res)
  #duracaocontrato<<-as.numeric(dlgInput("Duracao da vigencia contratual: ", Sys.info()["user"])$res)
  #pagamento<<-as.numeric(dlgInput("Valor do(s) pagamentos: ", Sys.info()["user"])$res)
  coefreversao<<-as.numeric(dlgInput("Coeficiente de reversao(em decimal): ", Sys.info()["user"])$res)
}

input() #chamar funcao com os inputs

#local do arquivo com a planilha com as tabuas
library(readxl)
tabuainput=read_excel("D:/BrowserDownloads/EAC0424_T_GRUPOXX_TABUAS.xlsx")

#agemax=tail(max(which(!is.na(tabuainput[,1]))),n=1) #ultima idade da tabua utilizada

#inicializacao dos vetores de comutacao, podendo-se alterar l0 e tamanho max de cada vetor(agemax)
variaveis=function(){
  agemax<<-119
  l0<<-10000000
  idade<<-c(0:agemax)
  l<<-vector(length=agemax+1) #estoque de vidas
  d<<-vector(length=agemax+1) #numero de mortes no periodo
  vx<<-vector(length=agemax+1) #desconto a valor presente
  Dx<<-vector(length=agemax+1) #valores a serem pagos por vida
  N<<-vector(length=agemax+1)
  Cx<<-vector(length=agemax+1)
  M<<-vector(length=agemax+1)
  p<<-vector(length=agemax+1)
  qx<<-vector(length=agemax+1)
  variaveis
}

variaveis()  #chamada da funcao para criacao dos vetores
  

if(nvidas==2){
  #criando dataframes
  duasvidasX=data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasY=data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasXY=data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  #renomear colunas
  colnames(duasvidasX)=c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(duasvidasY)=c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(duasvidasXY)=c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  #agemax=tail(duasvidasX[,1],n=1)
}
if(nvidas==3){
  #criando dataframes
  tresvidasX=data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasY=data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasZ=data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasXY=data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasYZ=data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXZ=data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXYZ=data.frame(idade,p,l,d,vx,Dx,N,C,M)
  #renomear colunas
  colnames(tresvidasX)=c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(tresvidasY)=c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(tresvidasZ)=c("Idade","qz","lz","dz","v^z","Dz","Nz","Cz","Mz")
  colnames(tresvidasXY)=c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  colnames(tresvidasYZ)=c("Idade","pyz","lyz","dyz","v^t","Dyz","Nyz","Cyz","Myz")
  colnames(tresvidasXZ)=c("Idade","pxz","lxz","dxz","v^t","Dxz","Nxz","Cxz","Mxz")
  colnames(tresvidasXYZ)=c("Idade","pxyz","lxyz","dxyz","v^t","Dxyz","Nxyz","Cxyz","Mxyz")
  #agemax=tail(tresvidasX[,1],n=1)
}

selecttable=function(Sexo,TipoTabua){
  if(Sexo=="M" && TipoTabua==1){tabua=c(tabuainput[,2])}
  else if(Sexo=="F" && TipoTabua==1){tabua=tabuainput[,1]}
  else if(Sexo=="M" && TipoTabua==2){tabua=tabuainput[,4]}
  else if(Sexo=="F" && TipoTabua==2){tabua=tabuainput[,3]}
  else if(Sexo=="M" && TipoTabua==3){tabua=tabuainput[,6]}
  else if(Sexo=="F" && TipoTabua==3){tabua=tabuainput[,5]}
}

#alocar tabuas para os individuos escolhidos
tabuaX=vector(length=agemax)
tabuaY=vector(length=agemax)
tabuaZ=vector(length=agemax)

tabuaX=selecttable(SexoTitular,TipoTabua)
tabuaY=selecttable(SexoConjuge,TipoTabua)
tabuaZ=selecttable(SexoDependente,TipoTabua)



#funcoes para completar as tabuas
#funcao para 1 vida
UmaVida=function(umavida,taxajuros,tabua,l0){
  umavida[2]=tabua #tabua escolhida
  umavida[1,3]=l0 #l0
  umavida[1,4]=umavida[1,2]*umavida[1,3] #d0
  for(i in 2:116){umavida[i,3]=umavida[i-1,3]-umavida[i-1,4] #lx
  umavida[i,4]=umavida[i,2]*umavida[i,3]} #dx
  umavida[5]=(1/(1+taxajuros)^umavida[1])#v^t
  umavida[6]=umavida[3]*umavida[5] #Dx
  for(i in 1:116){umavida[i,7]=sum(umavida[i:116,6])}#NX
  for(i in 1:116){umavida[i,8]=umavida[i,4]*umavida[i+1,5]} #Cx
  for(i in 1:116){umavida[i,9]=sum(umavida[i:116,8])} #Mx
  umavida 
}

#tabuas de 1 vida 
duasvidasX=UmaVida(duasvidasX,taxajuros=taxajuros,tabua=tabuaX,l0=l0)
duasvidasY=UmaVida(duasvidasY,taxajuros,tabua=tabuaY,l0=l0)

tresvidasX=UmaVida(tresvidasX,taxajuros,tabuaX,l0)
tresvidasY=UmaVida(tresvidasY,taxajuros,tabuaY,l0)
tresvidasZ=UmaVida(tresvidasZ,taxajuros,tabuaZ,l0)


#max(which(!is.na(x))) index do ultimo valor nao nulo em x

#funcao para 2 vidas
DuasVidas=function(duasvidas,duasvidasX,duasvidasY,taxajuros,l0){
  #fim=tail(tabua[,1],n=1)
  for(i in 1:116){
    duasvidas[i-1,2]=(1-duasvidasX[IdadeTitular+i-1,2])*(1-duasvidasY[IdadeConjuge+i-1,2])}
  duasvidas[1,3]=l0
  duasvidas[1,4]=(1-duasvidas[1,2])*duasvidas[1,3] #d0
  for(i in 2:116){duasvidas[i,3]=duasvidas[i-1,3]-duasvidas[i-1,4] #lx
  duasvidas[i,4]=(1-duasvidas[i,2])*duasvidas[i,3]} #dx
  duasvidas[5]=(1/(1+taxajuros)^duasvidas[1])#v^t
  duasvidas[6]=duasvidas[3]*duasvidas[5] #Dx
  for(i in 1:max(which(!is.na(duasvidas[2])))){duasvidas[i,7]=sum(duasvidas[i:max(which(!is.na(duasvidas[2]))),6])}#NX
  for(i in 1:116){duasvidas[i,8]=duasvidas[i,4]*duasvidas[i+1,5]} #Cx
  for(i in 1:max(which(!is.na(duasvidas[2])))){duasvidas[i,9]=sum(duasvidas[i:max(which(!is.na(duasvidas[2]))),8])} #Mx
  duasvidas 
}


#tabuas 2 vidas
duasvidasXY=DuasVidas(duasvidasXY,duasvidasX,duasvidasY,taxajuros,l0)
tresvidasXY=DuasVidas(tresvidasXY,tresvidasX,tresvidasY,taxajuros,l0)
tresvidasYZ=DuasVidas(tresvidasYZ,tresvidasY,tresvidasZ,taxajuros,l0)
tresvidasXZ=DuasVidas(tresvidasXZ,tresvidasX,tresvidasZ,taxajuros,l0)

#funcao para 3 vidas
TresVidas=function()
  
tresvidasXYZ=TresVidas(tresvidasXYZ,tresvidasX,tresvidasY,tresvidasZ,tresvidasXY,tresvidasXZ,tresvidasYZ,taxajuros,l0)

#calculo 1 vida (falta definir n)

#antecipada
anuidadeVitAntec=valorfacebene*umavida[Idade,7]/umavida[Idade,6]
anuidadeTempAntec=valorfacebene*(umavida[Idade,7]-umavida[Idade+n,7])/umavida[Idade,6]
anuidadeVitDifAntec=valorfacebene*(umavida[Idade+n,7])/umavida[Idade,6]
#postecipada
anuidadeVitPos=valorfacebene*umavida[Idade+1,7]/umavida[Idade,6]
anuidadeTempPos=valorfacebene*(umavida[Idade+1,7]-umavida[Idade+n+1,7])/umavida[Idade,6]
anuidadeVitDifPos=valorfacebene*(umavida[Idade+n+1,7])/umavida[Idade,6]
#Seguros Vida PUP
SeguroVida=valorfacebene*umavida[Idade,9]/umavida[Idade,6]
SeguroVidaDif=valorfacebene*umavida[Idade+n,9]/umavida[Idade,6]
SeguroVidaTemp=valorfacebene*(umavida[Idade,9]-umavida[Idade+n,9])/umavida[Idade,6]
#seguros vida anuais puros nivelados
SeguroVidaAnual=valorfacebene*umavida[Idade,9]/umavida[Idade,7]
SeguroVidaTemp=valorfacebene*(umavida[Idade,9]-umavida[Idade+n,9])/umavida[Idade,6]/(umavida[Idade,7]-umavida[Idade+n,7])/umavida[Idade,6]

#calculo 2 vidas X = Idade1 = maior e Y = Idade 2 = menor
Vidasconjuntas=valorfacebene*(duasvidas[1,7]-duasvidas[n,7])/duasvidas[1,6]
anuidadeX=valorfacebene*(duasvidas[Idade1,7]-duasvidas[Idade1+n,7])/duasvidas[Idade1,6]
anuidadeY=valorfacebene*(duasvidas[Idade2,7]-duasvidas[Idade2+n,7])/duasvidas[Idade2,6]
UltimoSobrevivente=anuidadeX+anuidadeY-Vidasconjuntas
AnuidadeReversaoXY=(anuidadeY-Vidasconjuntas)*coefreversao #de X para Y
AnuidadeReversaoYX=(anuidadeX-Vidasconjuntas)*coefreversao #de Y para X

#calculo 3 vidas
