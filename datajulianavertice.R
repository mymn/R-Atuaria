calendr="2022/06/01"
as.Date(calendr, "%y/%m/%d")
View(calendr)

#ano/mes/dia
Tocalendar=function(calendr){ #tranformar data juliana em data(dd/mm/aaaa)
  calendr=as.Date(calendr, origin="1900/01/01")
}

#ano/mes/dia
Tojulian=function(calendr){ #transformar data em data juliana
  calendr=julian.Date(calendr,origin=as.Date("1900-01-01"))
}


#criacao do vetor de fluxos
nfluxos=5 #numero de fluxos
fluxo=matrix(nrow=nfluxos,ncol=2)#[tempo,valor]tempo em data juliana
colnames(fluxo) <- c("Vencimento data juliana", "Valor")

#criacao do vetor de vertices
nvertices=5 #numero de vertices
vertices=matrix(nrow=nvertices,ncol=2) #[tempo,valor]
colnames(vertices) <- c("Data juliana", "Valor")

#vetor teste para fluxo
fluxo[,1]=c(45500,45700,46000,46100,45550)
fluxo[,2]=c(1,2,3,4,5)

#preencher os vertices para teste
vertices[,1]=c(44710,45075,45440,45805,46170) #inserir as datas
vertices[,2]=0

library(Rfast)
#vetor auxiliar para armazenar as "distancias" entre as datas de 
#vencimento dos fluxos e dos vertices
aux=vector(length=nvertices)
distribuicao=function(nfluxos,nvertices,vertices,fluxo){
  for(j in 1:nfluxos){
    aux=numeric(nvertices)
    for(i in 1:nvertices){
      aux[i]=as.numeric(abs(fluxo[j,1]-vertices[i,1]))
      print(aux)
      #indices dos 2 menores valores no vetor auxiliar(a,b)
      b=nth(as.numeric(aux), 2, descending = F,na.rm=T,index.return=T)
      a=nth(as.numeric(aux), 1, descending = F,na.rm=T,index.return=T)}
      #distribuicao proporcional nos vertices
      vertices[b,2]=vertices[b,2]+((abs(fluxo[j,1]-vertices[a,1]))/abs((vertices[a,1]-vertices[b,1])))*fluxo[j,2]
      vertices[a,2]=vertices[a,2]+((abs(fluxo[j,1]-vertices[b,1]))/abs((vertices[a,1]-vertices[b,1])))*fluxo[j,2]
  }
}

carteira=matrix(ncol=3,nrows=nfluxos)
colnames(carteira)=c("Dias Uteis compra ao venc","VF no venc","Dias Uteis hoje ao venc")

hoje=Tojulian(Sys.Date())

#carteira[1,]= dias compra -> vencimento
#carteira[2,]=valor ao final do vencimento
#carteira[3,]=dias hoje -> vencimento


calculocarteira=function(){
  hoje=Tojulian(Sys.Date())
  for(i in 1:nfluxos){
    if(titulospre$Produto=="LTN"){
      carteira[1,i]=Tojulian(titulospre[6,i])-Tojulian(titulospre[3,i])*252/365
      carteira[3,i]=Tojulian(titulospre[6,i])-Tojulian(hoje)*252/365
      carteira[2,i]=titulospre[4,i]*1000
    }
  }
}

fluxo[i,1]=vencimento[í]
fluxo[i,2]=carteira[2,i]*(1/(1+rtanual[í]^carteira[3,i]))

curva=function(){
  Selic=0.1275
  banual=0.15
  bdiario=(1+banual)^(1/252)-1
  #a = aceleracao convergencia media
  #b = longo prazo
  #sigma(S) = volatilidade da taxa de juros
  a=0.00002
  b=bdiario
  S=0.0002
  rt=vector(length=1000)
  rt1=vector(length=1000)
  rtanual=vector(length=1000)
  rt[1]=(1+Selic)^(1/252)-1
  for(i in 2:1000){
    rt[i]=rt[i-1]+a*(b-rt[i-1])+S*(rt[i-1]^0.5)*qnorm(runif(1,min=0,max=1))
  }
  #plot(rt)
  rt1=1+rt
  for(i in 1:1000){
    rtanual[i]=prod(rt1[1:i],na.rm = TRUE)^(252/i)-1
  }
  plot(rtanual)
}

plot(vertices[,2])
