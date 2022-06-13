#Titular X, Conjuge Y, Dependente Z
##colunas impares das tabuas F, pares M. 

#Instrucoes
#1-alterar o caminho na variavel tabuainput na funcao selecttable para o local da
#planilha com as tabuas de mortalidade
#2-ler todas funcoes do script
#3-rodar funcoes main no final do script 

library(svDialogs)
library(readxl)

#funcao com os inputs
input=function(){ 
  nvidas<<-as.numeric(dlgInput("Numero de vidas(2 ou 3): ", Sys.info()["user"])$res)
  IdadeTitular<<-as.numeric(dlgInput("Idade do titular ", Sys.info()["user"])$res)
  IdadeConjuge<<-as.numeric(dlgInput("Idade do conjuge ", Sys.info()["user"])$res)
  SexoTitular<<-as.character(dlgInput("Sexo do titular (M ou F) ", Sys.info()["user"])$res)
  SexoConjuge<<-as.character(dlgInput("Sexo do conjuge (M ou F) ", Sys.info()["user"])$res)
  if(nvidas==3){
    SexoDependente<<-as.character(dlgInput("Sexo do dependente (M ou F) ", Sys.info()["user"])$res)
    IdadeDependente<<-as.numeric(dlgInput("Idade do dependente (0 a 24) ", Sys.info()["user"])$res)}
  taxajuros<<-as.numeric(dlgInput("Taxa de juros (decimal) ", Sys.info()["user"])$res)
  TipoTabua<<-as.numeric(dlgInput("Tabua(1=AT2000,2=BR-EMSmt-2015,3=BR-EMSmt-2021): ", Sys.info()["user"])$res)
  Diferimento<<-as.numeric(dlgInput("Diferimento em anos: ", Sys.info()["user"])$res)
  valorfacebene<<-as.numeric(dlgInput("Valor de face do beneficio: ", Sys.info()["user"])$res)
  duracaocontrato<<-as.numeric(dlgInput("Duracao da vigencia contratual(anos): ", Sys.info()["user"])$res)
  pagamento<<-as.numeric(dlgInput("Duracao do(s) pagamentos(anos): ", Sys.info()["user"])$res)
  pensaomorte<<-as.numeric(dlgInput("Pensao por morte(sim=1, nao=0): ", Sys.info()["user"])$res)
  if(pensaomorte==1){
    coefreversao<<-as.numeric(dlgInput("Coeficiente de reversao(em decimal): ", Sys.info()["user"])$res)}
  lastsurvivor<<-as.numeric(dlgInput("Situacao de ultimo sobrevivente(sim=1, nao=0): ", Sys.info()["user"])$res)
  jointlives<<-as.numeric(dlgInput("Situacao de vidas conjuntas(sim=1, nao=0): ", Sys.info()["user"])$res)
  
}

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
  #alocar tabuas para os individuos escolhidos
  tabuaX<<-vector(length=agemax+1)
  tabuaY<<-vector(length=agemax+1)
  tabuaZ<<-vector(length=agemax+1)
  #criando dataframes
  duasvidasX<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasY<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  duasvidasXY<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  #renomear colunas
  colnames(duasvidasX)<<-c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(duasvidasY)<<-c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(duasvidasXY)<<-c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  
  #criando dataframes
  tresvidasX<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasY<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasZ<<-data.frame(idade,qx,l,d,vx,Dx,N,Cx,M)
  tresvidasXY<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasYZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  tresvidasXYZ<<-data.frame(idade,p,l,d,vx,Dx,N,Cx,M)
  #renomear colunas
  colnames(tresvidasX)<<-c("Idade","qx","lx","dx","v^x","Dx","Nx","Cx","Mx")
  colnames(tresvidasY)<<-c("Idade","qy","ly","dy","v^y","Dy","Ny","Cy","My")
  colnames(tresvidasZ)<<-c("Idade","qz","lz","dz","v^z","Dz","Nz","Cz","Mz")
  colnames(tresvidasXY)<<-c("Idade","pxy","lxy","dxy","v^t","Dxy","Nxy","Cxy","Mxy")
  colnames(tresvidasYZ)<<-c("Idade","pyz","lyz","dyz","v^t","Dyz","Nyz","Cyz","Myz")
  colnames(tresvidasXZ)<<-c("Idade","pxz","lxz","dxz","v^t","Dxz","Nxz","Cxz","Mxz")
  colnames(tresvidasXYZ)<<-c("Idade","pxyz","lxyz","dxyz","v^t","Dxyz","Nxyz","Cxyz","Mxyz")
}

#funcao para selecionar tabua
selecttable=function(Sexo){
  #local do arquivo com a planilha com as tabuas
  tabuainput=read_excel("D:/BrowserDownloads/EAC0424_T_GRUPOXX_TABUAS.xlsx")
  if(Sexo=="M" && TipoTabua==1){tabua=tabuainput[,2]}
  else if(Sexo=="F" && TipoTabua==1){tabua=tabuainput[,1]}
  else if(Sexo=="M" && TipoTabua==2){tabua=tabuainput[,4]}
  else if(Sexo=="F" && TipoTabua==2){tabua=tabuainput[,3]}
  else if(Sexo=="M" && TipoTabua==3){tabua=tabuainput[,6]}
  else if(Sexo=="F" && TipoTabua==3){tabua=tabuainput[,5]}
}

tabuaXYZ=function(){
  tabuaX<<-selecttable(SexoTitular)
  tabuaY<<-selecttable(SexoConjuge)
    if(nvidas==3){tabuaZ<<-selecttable(SexoDependente)}
}

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

#funcao para 2 vidas
DuasVidas=function(duasvidas,duasvidas1,duasvidas2,taxajuros,l0,Idade1,Idade2){
  #fim=tail(tabua[,1],n=1)
  for(i in 1:116){
    duasvidas[i-1,2]=(1-duasvidas1[Idade1+i-1,2])*(1-duasvidas2[Idade2+i-1,2])}
  duasvidas[1,3]=l0
  duasvidas[1,4]=(1-duasvidas[1,2])*duasvidas[1,3] #d0
  for(i in 2:116){duasvidas[i,3]=duasvidas[i-1,3]-duasvidas[i-1,4] #lx
  duasvidas[i,4]=(1-duasvidas[i,2])*duasvidas[i,3]} #dx
  duasvidas[5]=(1/(1+taxajuros)^duasvidas[1])#v^t
  duasvidas[6]=duasvidas[3]*duasvidas[5] #Dx
  for(i in 1:116){duasvidas[i,7]=sum(duasvidas[i:116,6],na.rm=TRUE)}#NX
  for(i in 1:116){duasvidas[i,8]=duasvidas[i,4]*duasvidas[i+1,5]} #Cx
  for(i in 1:116){duasvidas[i,9]=sum(duasvidas[i:116,8],na.rm=TRUE)} #Mx
  duasvidas 
}

#funcao para 3 vidas
TresVidas=function(tresvidas,tresvidasX,tresvidasY,tresvidasZ,taxajuros,l0){
  for(i in 1:116){
    tresvidas[i-1,2]=(1-tresvidasX[IdadeTitular+i-1,2])*(1-tresvidasY[IdadeConjuge+i-1,2])*(1-tresvidasZ[IdadeDependente+i-1,2])}
  tresvidas[1,3]=l0
  tresvidas[1,4]=(1-tresvidas[1,2])*tresvidas[1,3] #d0
  for(i in 2:116){tresvidas[i,3]=tresvidas[i-1,3]-tresvidas[i-1,4] #lx
  tresvidas[i,4]=(1-tresvidas[i,2])*tresvidas[i,3]} #dx
  tresvidas[5]=(1/(1+taxajuros)^tresvidas[1])#v^t
  tresvidas[6]=tresvidas[3]*tresvidas[5] #Dx
  for(i in 1:116){tresvidas[i,7]=sum(tresvidas[i:116,6],na.rm=TRUE)}#NX
  for(i in 1:116){tresvidas[i,8]=tresvidas[i,4]*tresvidas[i+1,5]} #Cx
  for(i in 1:116){tresvidas[i,9]=sum(tresvidas[i:116,8],na.rm=TRUE)} #Mx
  tresvidas
}

#funcao para o calculo das precificacoes
calculo=function(){
  resultado=matrix(nrow=7,ncol=14)
  colnames(resultado)=c("AnuidadeVitAntec","AnuidadeTempAntec","AnuidadeVitDifAntec","AnuidadeVitPos","AnuidadeTempPos","AnuidadeVitDifPos","SeguroVida","SeguroVidaDif","SeguroVidaTemp","VidasConjuntas","UltimoSobrevivente","Reversao","Dotal Puro","Dotal Misto")
  rownames(resultado)=c("X","Y","Z","XY","XZ","YZ","XYZ")
  #anuidade vitalicia antecipada
  resultado[1,1]=valorfacebene*duasvidasX[IdadeTitular+1,7]/duasvidasX[IdadeTitular+1,6]
  resultado[2,1]=valorfacebene*duasvidasY[IdadeConjuge+1,7]/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,1]=valorfacebene*tresvidasZ[IdadeDependente+1,7]/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,1]=valorfacebene*duasvidasXY[1,7]/duasvidasXY[1,6]
  if(nvidas==3){
    resultado[5,1]=valorfacebene*tresvidasXZ[1,7]/tresvidasXZ[1,6]
    resultado[6,1]=valorfacebene*tresvidasYZ[1,7]/tresvidasYZ[1,6]
    resultado[7,1]=valorfacebene*tresvidasXYZ[1,7]/tresvidasXYZ[1,6]}
  #anuidade temporaria de n anos antecipada
  resultado[1,2]=valorfacebene*(duasvidasX[IdadeTitular+1,7]-duasvidasX[IdadeTitular+Diferimento+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,2]=valorfacebene*(duasvidasX[IdadeConjuge+1,7]-duasvidasY[IdadeConjuge+Diferimento+1,7])/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,2]=valorfacebene*(tresvidasZ[IdadeDependente+1,7]-tresvidasZ[IdadeDependente+Diferimento+1,7])/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,2]=valorfacebene*(duasvidasXY[IdadeTitular+1,7]-duasvidasXY[IdadeTitular+Diferimento+IdadeConjuge+1,7])/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){    
    resultado[5,2]=valorfacebene*(tresvidasXZ[IdadeTitular+1,7]-tresvidasXZ[IdadeTitular+Diferimento+IdadeDependente+1,7])/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,2]=valorfacebene*(tresvidasYZ[IdadeConjuge+1,7]-tresvidasYZ[IdadeConjuge+Diferimento+IdadeDependente+1,7])/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,2]=valorfacebene*(tresvidasXYZ[IdadeDependente+1,7]-tresvidasXYZ[IdadeDependente+1,7])/tresvidasXYZ[IdadeDependente+1,6]}
  
  #anuidade vitalicia diferida em n antecipada
  resultado[1,3]=valorfacebene*(duasvidasX[IdadeTitular+Diferimento+1,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,3]=valorfacebene*(duasvidasY[IdadeConjuge+Diferimento+1,7])/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,3]=valorfacebene*(tresvidasZ[IdadeDependente+Diferimento+1,7])/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,3]=valorfacebene*(duasvidasXY[IdadeTitular+Diferimento+1,7])/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){
    resultado[5,3]=valorfacebene*(tresvidasXZ[IdadeTitular+Diferimento+1,7])/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,3]=valorfacebene*(tresvidasYZ[IdadeConjuge+Diferimento+1,7])/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,3]=valorfacebene*(tresvidasXYZ[IdadeTitular+Diferimento+1,7])/tresvidasXYZ[IdadeTitular+1,6]}
  
  #anuidade vitalicia postecipada 
  resultado[1,4]=valorfacebene*duasvidasX[IdadeTitular+2,7]/duasvidasX[IdadeTitular+1,6]
  resultado[2,4]=valorfacebene*duasvidasY[IdadeConjuge+2,7]/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,4]=valorfacebene*tresvidasZ[IdadeDependente+2,7]/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,4]=valorfacebene*duasvidasXY[2,7]/duasvidasXY[1,6]
  if(nvidas==3){
    resultado[5,4]=valorfacebene*tresvidasXZ[2,7]/tresvidasXZ[1,6]
    resultado[6,4]=valorfacebene*tresvidasYZ[2,7]/tresvidasYZ[1,6]
    resultado[7,4]=valorfacebene*tresvidasXYZ[2,7]/tresvidasXYZ[1,6]}
  
  #anuidade temporaria de n anos postecipada
  resultado[1,5]=valorfacebene*(duasvidasX[IdadeTitular+2,7]-duasvidasX[IdadeTitular+Diferimento+2,7])/duasvidasX[IdadeTitular+1,6]
  resultado[2,5]=valorfacebene*(duasvidasY[IdadeConjuge+2,7]-duasvidasY[IdadeConjuge+Diferimento+2,7])/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){  
    resultado[3,5]=valorfacebene*(tresvidasZ[IdadeDependente+2,7]-tresvidasZ[IdadeDependente+Diferimento+2,7])/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,5]=valorfacebene*(duasvidasXY[IdadeTitular+2,7]-duasvidasXY[IdadeTitular+Diferimento+2,7])/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){
    resultado[5,5]=valorfacebene*(tresvidasXZ[IdadeTitular+2,7]-tresvidasXZ[IdadeTitular+Diferimento+2,7])/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,5]=valorfacebene*(tresvidasYZ[IdadeConjuge+2,7]-tresvidasYZ[IdadeConjuge+Diferimento+2,7])/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,5]=valorfacebene*(tresvidasXYZ[IdadeTitular+2,7]-tresvidasXYZ[IdadeTitular+Diferimento+2,7])/tresvidasXYZ[IdadeTitular+1,6]
  }
  
  #anuidade vitalicia diferida em n postecipada
  resultado[1,6]=valorfacebene*duasvidasX[IdadeTitular+Diferimento+2,7]/duasvidasX[IdadeTitular+1,6]
  resultado[2,6]=valorfacebene*duasvidasY[IdadeConjuge+Diferimento+2,7]/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,6]=valorfacebene*tresvidasZ[IdadeDependente+Diferimento+2,7]/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,6]=valorfacebene*duasvidasXY[IdadeTitular+Diferimento+2,7]/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){
    resultado[5,6]=valorfacebene*tresvidasXZ[IdadeTitular+Diferimento+2,7]/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,6]=valorfacebene*tresvidasYZ[IdadeConjuge+Diferimento+2,7]/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,6]=valorfacebene*tresvidasXYZ[IdadeTitular+Diferimento+2,7]/tresvidasXYZ[IdadeTitular+1,6]
  }
  
  #Seguro Vida Inteira PUP
  resultado[1,7]=valorfacebene*(duasvidasX[IdadeTitular+1,9])/(duasvidasX[IdadeTitular+1,6])
  resultado[2,7]=valorfacebene*(duasvidasY[IdadeConjuge+1,9])/(duasvidasY[IdadeConjuge+1,6])
  if(nvidas==3){
    resultado[3,7]=valorfacebene*(tresvidasZ[IdadeDependente+1,9])/(tresvidasZ[IdadeDependente+1,6])}
  resultado[4,7]=valorfacebene*(duasvidasXY[IdadeTitular+1,9])/(duasvidasXY[IdadeTitular+1,6])
  if(nvidas==3){
    resultado[5,7]=valorfacebene*(tresvidasXZ[IdadeTitular+1,9])/(tresvidasXZ[IdadeTitular+1,6])
    resultado[6,7]=valorfacebene*(tresvidasYZ[IdadeConjuge+1,9])/(tresvidasYZ[IdadeConjuge+1,6])
    resultado[7,7]=valorfacebene*(tresvidasXYZ[IdadeTitular+1,9])/(tresvidasXYZ[IdadeTitular+1,6])}
  
  #Seguro Vida Diferido em n anos PUP
  resultado[1,8]=valorfacebene*(duasvidasX[IdadeTitular+Diferimento+1,9])/duasvidasX[IdadeTitular+1,6]
  resultado[2,8]=valorfacebene*(duasvidasY[IdadeConjuge+Diferimento+1,9])/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,8]=valorfacebene*(tresvidasZ[IdadeDependente+Diferimento+1,9])/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,8]=valorfacebene*(duasvidasXY[IdadeTitular+Diferimento+1,9])/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){
    resultado[5,8]=valorfacebene*(tresvidasXZ[IdadeTitular+Diferimento+1,9])/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,8]=valorfacebene*(tresvidasYZ[IdadeConjuge+Diferimento+1,9])/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,8]=valorfacebene*(tresvidasXYZ[IdadeTitular+Diferimento+1,9])/tresvidasXYZ[IdadeTitular+1,6]
  }
  
  #Seguro Vida Temporario n anos PUP
  resultado[1,9]=valorfacebene*(duasvidasX[IdadeTitular+1,9]-duasvidasX[IdadeTitular+Diferimento+1,9])/duasvidasX[IdadeTitular+1,6]
  resultado[2,9]=valorfacebene*(duasvidasY[IdadeConjuge+1,9]-duasvidasY[IdadeConjuge+Diferimento+1,9])/duasvidasY[IdadeConjuge+1,6]
  if(nvidas==3){
    resultado[3,9]=valorfacebene*(tresvidasZ[IdadeDependente+1,9]-tresvidasZ[IdadeDependente+Diferimento+1,9])/tresvidasZ[IdadeDependente+1,6]}
  resultado[4,9]=valorfacebene*(duasvidasXY[IdadeTitular+1,9]-duasvidasXY[IdadeTitular+Diferimento+1,9])/duasvidasXY[IdadeTitular+1,6]
  if(nvidas==3){
    resultado[5,9]=valorfacebene*(tresvidasXZ[IdadeTitular+1,9]-tresvidasXZ[IdadeTitular+Diferimento+1,9])/tresvidasXZ[IdadeTitular+1,6]
    resultado[6,9]=valorfacebene*(tresvidasYZ[IdadeConjuge+1,9]-tresvidasYZ[IdadeConjuge+Diferimento+1,9])/tresvidasYZ[IdadeConjuge+1,6]
    resultado[7,9]=valorfacebene*(tresvidasXYZ[IdadeTitular+1,9]-tresvidasXYZ[IdadeTitular+Diferimento+1,9])/tresvidasXYZ[IdadeTitular+1,6]
  }
  
  #Seguro Dotal Puro PUP
  resultado[1,13]=valorfacebene*(duasvidasX[IdadeTitular+Diferimento,6]/duasvidasX[IdadeTitular,6])
  resultado[2,13]=valorfacebene*(duasvidasY[IdadeConjuge+Diferimento,6]/duasvidasY[IdadeConjuge,6])
  if(nvidas==3){resultado[3,13]=valorfacebene*(tresvidasZ[IdadeDependente+Diferimento,6]/tresvidasZ[IdadeDependente,6])}
  resultado[4,13]=valorfacebene*(duasvidasXY[IdadeTitular+Diferimento,6]/duasvidasXY[IdadeTitular,6])
  if(nvidas==3){
    resultado[5,13]=valorfacebene*(tresvidasXZ[IdadeTitular+Diferimento,6]/tresvidasXZ[IdadeTitular,6])
    resultado[6,13]=valorfacebene*(tresvidasYZ[IdadeConjuge+Diferimento,6]/tresvidasYZ[IdadeConjuge,6])
    resultado[7,13]=valorfacebene*(tresvidasXYZ[IdadeTitular+Diferimento,6]/tresvidasXYZ[IdadeTitular,6])
  }
  
  #Seguro Dotal Misto PUP
  resultado[1,14]=valorfacebene*(duasvidasX[IdadeTitular,9]-duasvidasX[IdadeTitular+Diferimento,9]+duasvidasX[IdadeTitular+Diferimento,6])/duasvidasX[IdadeTitular,6]
  resultado[2,14]=valorfacebene*(duasvidasY[IdadeConjuge,9]-duasvidasY[IdadeConjuge+Diferimento,9]+duasvidasY[IdadeConjuge+Diferimento,6])/duasvidasY[IdadeConjuge,6]
  if(nvidas==3){resultado[3,14]=valorfacebene*(tresvidasZ[IdadeDependente,9]-tresvidasZ[IdadeDependente+Diferimento,9]+tresvidasZ[IdadeDependente+Diferimento,6])}
  resultado[4,14]=valorfacebene*(duasvidasXY[IdadeTitular,9]-duasvidasXY[IdadeTitular+Diferimento,9]+duasvidasXY[IdadeTitular+Diferimento,6])/duasvidasXY[IdadeTitular,6]
  if(nvidas==3){
    resultado[5,14]=valorfacebene*(tresvidasXZ[IdadeTitular,9]-tresvidasXZ[IdadeTitular+Diferimento,9]+tresvidasXZ[IdadeTitular+Diferimento,6])/tresvidasXZ[IdadeTitular,6]
    resultado[6,14]=valorfacebene*(tresvidasYZ[IdadeConjuge,9]-tresvidasYZ[IdadeConjuge+Diferimento,9]+tresvidasYZ[IdadeConjuge+Diferimento,6])/tresvidasYZ[IdadeConjuge,6]
    resultado[7,14]=valorfacebene*(tresvidasXYZ[IdadeTitular,9]-tresvidasXYZ[IdadeTitular+Diferimento,9]+tresvidasXYZ[IdadeTitular+Diferimento,6])/tresvidasXYZ[IdadeTitular,6]
  }
  
  #Vidas conjuntas
  if(jointlives==1){
    resultado[4,10]=valorfacebene*duasvidasXY[1,7]/duasvidasXY[1,6]
    if(nvidas==3){
      resultado[5,10]=valorfacebene*tresvidasXZ[1,7]/tresvidasXZ[1,6]
      resultado[6,10]=valorfacebene*tresvidasYZ[1,7]/tresvidasYZ[1,6]
      resultado[7,10]=valorfacebene*tresvidasXYZ[1,7]/tresvidasXYZ[1,6]}
  }
  
  #Ultimo Sobrevivente
  if(lastsurvivor==1){
    resultado[4,11]=valorfacebene*(resultado[1,1]+resultado[2,1]-resultado[4,10])
    if(nvidas==3){
      resultado[5,11]=valorfacebene*(resultado[1,1]+resultado[3,1]-resultado[5,10])
      resultado[6,11]=valorfacebene*(resultado[2,1]+resultado[3,1]-resultado[6,10])}
  
  }
  
  #reversao X para Y
  resultado[4,12]=(resultado[2,1]-resultado[4,10])*coefreversao
  
  #reversao Y para X
  resultado[2,12]=(resultado[1,1]-resultado[4,10])*coefreversao
  resultado<<-resultado  
}

#funcao para preencher as tabuas
tableset=function(){
  #Uma vida
  duasvidasX<<-UmaVida(duasvidasX,taxajuros=taxajuros,tabua=tabuaX,l0=l0)
  duasvidasY<<-UmaVida(duasvidasY,taxajuros,tabua=tabuaY,l0=l0)
  tresvidasX<<-UmaVida(tresvidasX,taxajuros,tabuaX,l0)
  tresvidasY<<-UmaVida(tresvidasY,taxajuros,tabuaY,l0)
  tresvidasZ<<-UmaVida(tresvidasZ,taxajuros,tabuaZ,l0)
  #Duas vidas
  duasvidasXY<<-DuasVidas(duasvidasXY,duasvidas1=duasvidasX,duasvidas2=duasvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
  tresvidasXY<<-DuasVidas(tresvidasXY,duasvidas1=tresvidasX,duasvidas2=tresvidasY,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeConjuge)
  if(nvidas==3){
    tresvidasYZ<<-DuasVidas(tresvidasYZ,duasvidas1=tresvidasY,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeConjuge,Idade2=IdadeDependente)
    tresvidasXZ<<-DuasVidas(tresvidasXZ,duasvidas1=tresvidasX,duasvidas2=tresvidasZ,taxajuros,l0,Idade1=IdadeTitular,Idade2=IdadeDependente)}
  #Tres vidas
  if(nvidas==3){tresvidasXYZ<<-TresVidas(tresvidasXYZ,tresvidasX,tresvidasY,tresvidasZ,taxajuros,l0)}
  tableset
}

#funcoes "main"

main=function(){
  input()
  variaveis()
  tabuaXYZ()
}
main2=function(){
  tableset()
  calculo()
}

#funcoes a serem rodadas
main()
main2()
