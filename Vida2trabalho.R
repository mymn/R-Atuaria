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
TaxaJuros=readline(prompt = "Taxa de juros(decimal): ")
Tabua=readline(prompt = "Tabua(1=AT2000,2=B...,3=...): ")
