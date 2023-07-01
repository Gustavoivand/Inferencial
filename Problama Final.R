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
  
  vBeta1<-varianza/Sxx
  vBeta0<-varianza*sum(internalX^2)/(tamano_muestra*Sxx)
  
  sqrR<-(tss-rss)/tss
  
  Respuestas<-c(intbeta0,intbeta1,varianza,sqrR,vBeta0,vBeta1)
  Nombres<-c("beta0","beta1","varianza e","R cuadrado","varianza B0","varianza B1")
  
  Data_respuestas<-data.frame(Nombres, Respuestas)
  return(Data_respuestas)
}

confiance_beta0<-function(alfa,dframe){
  tamano_muestra<-length(as.vector(dframe$X))
  Valores_respuesta<-Trouver_lineaire(dframe)
  Beta0<-Valores_respuesta[1,2]
  desvBeta0<-sqrt(Valores_respuesta[5,2])
  
  glib<-tamano_muestra-2
  critico<-qt(1-(alfa/2),glib)
  
  margen<-critico*desvBeta0
  intervalo<-Beta0+c(-margen,margen)
  return(intervalo)
}

confiance_beta1<-function(alfa,dframe){
  tamano_muestra<-length(as.vector(dframe$X))
  Valores_respuesta<-Trouver_lineaire(dframe)
  Beta1<-Valores_respuesta[2,2]
  desvBeta1<-sqrt(Valores_respuesta[6,2])
  
  glib<-tamano_muestra-2
  critico<-qt(1-(alfa/2),glib)
  
  margen<-critico*desvBeta1
  intervalo<-Beta1+c(-margen,margen)
  return(intervalo)
}


#Datos a Calcular
X <- c(1, 4, 5, 9, 11, 13, 23, 23, 28)
Y <- c(64, 71, 54, 81, 76, 93, 77, 95, 109)
df <- data.frame(X, Y)


Respuestas_regresion<-Trouver_lineaire(df)
Respuestas_regresion

intervaloBeta0<-confiance_beta0(.05,df)
intervaloBeta0
intervaloBeta1<-confiance_beta1(.05,df)
intervaloBeta1



#calculo con lm
evaluacion.lm <- lm(formula = Y ~ X,
                  data = df)

summary(evaluacion.lm)

