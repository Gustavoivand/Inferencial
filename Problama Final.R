#Datos a Calcular
X <- c(-2,-1,0,1,2)
Y <- c(0,0,1,1,3)

df <- data.frame(X, Y)

#Función Beta1
Trouver_lineaire <- function(dframe) { 
  internalX<-as.vector(dframe$X)
  internalY<-as.vector(dframe$Y)
  tamano_muestra<-length(internalX)
  MediaX<-mean(internalX)
  MediaY<-mean(internalY)
  
  v.Sxy<-vector()
  v.Sxy<-(internalX-MediaX)*(internalY-MediaY)
  Sxy<-sum(v.Sxy)
  paste("Sxy:",Sxy)
  
  v.Sxx<-vector()
  v.Sxx<-(internalX-MediaX)*(internalX-MediaX)
  Sxx<-sum(v.Sxx)
  paste("Sxx:",Sxx)
  
  #Beta1
  intbeta1<-Sxy/Sxx
  #Beta0
  intbeta0<-MediaY-intbeta1*MediaX
  
  EstimadorY<-vector()
  EstimadorY<-intbeta0+intbeta1*internalX
  
  rss<-sum((internalY-EstimadorY)^2)
  tss<-sum((internalY-MediaY)^2)
  #varianza
  varianza<-rss/(tamano_muestra-2)
  
  sqrR<-(tss-rss)/tss
  
  Respuestas<-c(intbeta0,intbeta1,varianza,sqrR)
  Nombres<-c("beta0","beta1","varianza","R cuadrado")
  
  Data_respuestas<-data.frame(Nombres, Respuestas)
  return(Data_respuestas)
}


Valores_respuesta<-Trouver_lineaire(df)






#calculo con lm
evaluacion.lm <- lm(formula = Y ~ X,
                  data = df)

summary(evaluacion.lm)
evaluacion.lm$coefficients
