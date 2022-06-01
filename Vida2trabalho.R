#Titular X, Conjuge Y, Dependente Z
#9 colunas por combinacao
#63 colunas ou 55 colunas com idade reaproveitada
#1 dataframe para tabuas
#3 dataframes para 2 vidas
#7 dataframes para 3 vidas
#colnames(Dataframe) <- c("Idade", "q", "l", "d", "v^x", "D", "N", "C", "M")
#colunas impares das tabuas F, pares M. 
#Dataframe[,4]=Dataframe[+1,3]-Dataframe[,3]*Dataframe[,2]
#Primeiro dataframe:[Idade,qx,lx,dx,v^x,Dx,Nx,Cx,Mx]
#
#ex: tabua 1, sexo F-> tabua[,1] tabua 2, sexo M -> tabua[,4]
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
}
input() #chamar funcao com os inputs

library(readxl)
tabua=read_excel("D:/BrowserDownloads/EAC0424_T_GRUPOXX_TABUAS.xlsx")
agemax=tail(tabua[,1],n=1) #ultima idade da tabua utilizada
#inicializacao dos vetores de comutacao
agemax=120
l0=10000000
idade=c(0:agemax)
l=vector(length=agemax+1)
d=vector(length=agemax+1)
vx=vector(length=agemax+1)
D=vector(length=agemax+1)
N=vector(length=agemax+1)
C=vector(length=agemax+1)
M=vector(length=agemax+1)
p=vector(length=agemax+1)
q=vector(length=agemax+1)
idade

if(nvidas==2){
  #criando dataframes
  duasvidasX=data.frame(idade,q,l,d,vx,D,N,C,M)
  duasvidasY=data.frame(idade,q,l,d,vx,D,N,C,M)
  duasvidasXY=data.frame(idade,p,l,d,vx,D,N,C,M)
  #renomear colunas
  colnames(duasvidasX)=c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(duasvidasY)=c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(duasvidasXY)=c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  agemax=tail(duasvidasX[,1],n=1)
}
if(nvidas==3){
  #criando dataframes
  tresvidasX=data.frame(idade,q,l,d,vx,D,N,C,M)
  tresvidasY=data.frame(idade,q,l,d,vx,D,N,C,M)
  tresvidasZ=data.frame(idade,q,l,d,vx,D,N,C,M)
  tresvidasXY=data.frame(idade,p,l,d,vx,D,N,C,M)
  tresvidasYZ=data.frame(idade,p,l,d,vx,D,N,C,M)
  tresvidasXZ=data.frame(idade,p,l,d,vx,D,N,C,M)
  tresvidasXYZ=data.frame(idade,p,l,d,vx,D,N,C,M)
  #renomear colunas
  colnames(tresvidasX)=c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(tresvidasY)=c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(tresvidasZ)=c("Idade","qz","lz","dz","v^z","Dz","Nz","Cz","Mz")
  colnames(tresvidasXY)=c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  colnames(tresvidasYZ)=c("Idade","pyz","lyz","dyz","v^t","Dyz","Nyz","Cyz","Myz")
  colnames(tresvidasXZ)=c("Idade","pxz","lxz","dxz","v^t","Dxz","Nxz","Cxz","Mxz")
  colnames(tresvidasXYZ)=c("Idade","pxyz","lxyz","dxyz","v^t","Dxyz","Nxyz","Cxyz","Mxyz")
  agemax=tail(tresvidasX[,1],n=1)
}
tabua=c(length=121)
tabua=c(0.002311000,
             0.000906000,
             0.000504000,
             0.000408000,
             0.000357000,
             0.000324000,
             0.000301000,
             0.000286000,
             0.000328000,
             0.000362000,
             0.000390000,
             0.000413000,
             0.000431000,
             0.000446000,
             0.000458000,
             0.000470000,
             0.000481000,
             0.000495000,
             0.000510000,
             0.000528000,
             0.000549000,
             0.000573000,
             0.000599000,
             0.000627000,
             0.000657000,
             0.000686000,
             0.000714000,
             0.000738000,
             0.000758000,
             0.000774000,
             0.000784000,
             0.000789000,
             0.000789000,
             0.000790000,
             0.000791000,
             0.000792000,
             0.000794000,
             0.000823000,
             0.000872000,
             0.000945000,
             0.001043000,
             0.001168000,
             0.001322000,
             0.001505000,
             0.001715000,
             0.001948000,
             0.002198000,
             0.002463000,
             0.002740000,
             0.003028000,
             0.003330000,
             0.003647000,
             0.003980000,
             0.004331000,
             0.004698000,
             0.005077000,
             0.005465000,
             0.005861000,
             0.006265000,
             0.006694000,
             0.007170000,
             0.007714000,
             0.008348000,
             0.009093000,
             0.009968000,
             0.010993000,
             0.012188000,
             0.013572000,
             0.015160000,
             0.016946000,
             0.018920000,
             0.021071000,
             0.023388000,
             0.025871000,
             0.028552000,
             0.031477000,
             0.034686000,
             0.038225000,
             0.042132000,
             0.046427000,
             0.051128000,
             0.056250000,
             0.061809000,
             0.067826000,
             0.074322000,
             0.081326000,
             0.088863000,
             0.096958000,
             0.105631000,
             0.114858000,
             0.124612000,
             0.134861000,
             0.145575000,
             0.156727000,
             0.168290000,
             0.180245000,
             0.192565000,
             0.205229000,
             0.218683000,
             0.233371000,
             0.249741000,
             0.268237000,
             0.289305000,
             0.313391000,
             0.340940000,
             0.372398000,
             0.408210000,
             0.448823000,
             0.494681000,
             0.546231000,
             0.603917000,
             0.668186000,
             0.739483000,
             0.818254000,
             0.904945000,
             1.000000000)

a=function(){duasvidasX[,3]-duasvidasX[,4]}
#funcoes para completar as tabuas

fillonelife=function(onelife,taxajuros,tabua,l0){#optimizar remocao das linhas extras (baseado na tabua)
  onelife=onelife[-c(117:125),]
  onelife[,2]=tabua#qx
  onelife[1,3]=l0#l0
  for(i in 2:117){onelife[i,3]=onelife[i-1,3]-onelife[i-1,4]} #lx
  onelife[,4]=onelife[,2]*onelife[,3] #dx
  onelife[,5]=(1/(1+taxajuros)^onelife[,1])#v^t
  onelife[,6]=onelife[,3]*onelife[,5] #Dx
  for(i in 1:116){onelife[i,7]=sum(onelife[i,6]:onelife[116,6])}#NX
  for(i in 1:115){onelife[i,8]=onelife[i,4]*onelife[i+1,5]} #Cx
  for(i in 1:115){onelife[i,9]=sum(onelife[i,8]:onelife[115,8])} #Mx
  onelife=onelife[-c(117),]
}
duasvidasY=fillonelife(duasvidasY,taxajuros,tabua,l0)

filltwolives()
fillthreelives()
