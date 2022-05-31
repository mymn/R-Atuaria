#Titular X, Conjuge Y, Dependente Z
#9 colunas por combinacao
#63 colunas ou 55 colunas com idade reaproveitada
#1 dataframe para tabuas
#3 dataframes para 2 vidas
#7 dataframes para 3 vidas
#colnames(Dataframe) <- c("Idade", "q", "l", "d", "v^x", "D", "N", "C", "M")
#Unir dataframes da mesma categoria(ex 2 vidas), e
# deletar as colunas extras de idade e desconto
#Dataframe[,4]=Dataframe[+1,3]-Dataframe[,3]*Dataframe[,2]
#Primeiro dataframe:[Idade,qx,lx,dx,v^x,Dx,Nx,Cx,Mx]
#a partir disso: [qx,lx,dx,Dx,Nx,Cx,Mx]
#
#
#
#
IdadeTitular=readline(prompt = "Idade do titular: ")
IdadeConjuge=readline(prompt = "Idade do conjuge: ")
IdadeDependente=readline(prompt = "Idade do dependente: ")
SexoTitular=readline(prompt = "Sexo do titular(M ou F): ")
SexoConjuge=readline(prompt = "Sexo do conjuge(M ou F): ")
SexoDependente=readline(prompt = "Sexo do depentente(M ou F): ")
jaxajuros=readline(prompt = "Taxa de juros(decimal): ")
Tabua=readline(prompt = "Tabua(1=AT2000,2=B...,3=...): ")
nvidas=readline(prompt = "Numero de vidas(2 ou 3): ")

agemax=tail(tabua[,1],n=1) #ultima idade da tabua utilizada
#inicializacao dos vetores de comutacao
agemax=120
idade=c(1:agemax)
l=vector(length=agemax)
d=vector(length=agemax)
vx=vector(length=agemax)
D=vector(length=agemax)
N=vector(length=agemax)
C=vector(length=agemax)
M=vector(length=agemax)
p=vector(length=agemax)
q=vector(length=agemax)

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
