library(MASS)
library(readxl)
library(fastDummies)
library(SignifReg)
logit=function(){
  dummy<-read_excel("D:/BrowserDownloads/Avaliação_P01_bd.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  model1=glm(dummy$Perdas~.,data=dummy,family=binomial(link='logit'))
  modellogit=step(model1,direction='both',k=3.841459)
  summary(modellogit)
  chance=exp(cbind("Odds ratio"=coef(modellogit),confint.default(modellogit,level=0.95)))
}
library(pROC)
modelpoisson=function(){
  dummy<-read_excel("D:/BrowserDownloads/EAC 0355_Atividade 08_bd_poisson.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  dummy=dummy[,-c(4)]
  model1=glm(dummy$QTsinistro~.,data=dummy,family=poisson(link='log'))
  model2=step(model1,direction='both',k=3.841459)
  model2$coefficients=round(model2$coefficients,3)
  summary(model2)  
  model2$coefficients
  predicted=predict(model2,dummy,type='response')
  multiclass.roc(dummy$QTsinistro, predicted)
}
modelpoisson()
poisson=
#exp(resultado) - log

modelnormal=function(){
  dummy<-read_excel("D:/BrowserDownloads/EAC 0355_Atividade 08_bd_normal.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  model1=glm(dummy$Receita~.,data=dummy,family=gaussian(link='identity'))
  model2=step(model1,direction='both',k=3.841459)
  summary(model2)
  model2$coefficients
  chance=exp(cbind("Odds ratio"=coef(model2),confint.default(model2,level=0.95)))
  chance
  predict(model2,)
}


modelbernoulli=function(){
  dummy<-read_excel("D:/BrowserDownloads/EAC 0355_Atividade 08_bd_bernoulli.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  dummy$Sexo_feminino=0
  dummy=dummy[,-c(2)]
  model1=glm(dummy$Default_sim~.,data=dummy,family=binomial(link='logit'))
  model2=step(model1,direction='both',k=3.841459)
  summary(model2)  
  model2$coefficients
  chance=exp(cbind("Odds ratio"=coef(model2),confint.default(model2,level=0.95)))
  chance
}
modelbernoulli()
#(exp(resultado))/(1+exp(resultado))

modelgama=function(){
  dummy<-read_excel("D:/BrowserDownloads/EAC 0355_Atividade 09_bd.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  model1=glm(dummy$ValorPago~.,data=dummy,family=Gamma(link=log))
  model2=step(model1,direction='both',k=3.841459)
  summary(model2)
  chance=exp(cbind("Odds ratio"=coef(model2),confint.default(model2,level=0.95)))
  chance
  predict(model2,)
}
modelnormalinv=function(){
  dummy<-read_excel("D:/BrowserDownloads/EAC 0355_Atividade 09_bd.xlsx")
  dummy=dummy_cols(dummy,remove_selected_columns = T)
  dummy=dummy[,-c(6,19,21)]
  model1=glm(dummy$ValorPago~.,data=dummy,family=inverse.gaussian(link="log"))
  model1
  model2=step(model1,direction='both',k=3.841459)
  summary(model2)
  chance=exp(cbind("Odds ratio"=coef(model2),confint.default(model2,level=0.95)))
  chance
  predict(model2,)
  model2$coefficients
  anova(model2,test="Chisq") 
}
#para valor de k = qchisq(0.05,1,lower.tail=FALSE)
#summary
#AIC 
#Residual deviance = Função desvio
#