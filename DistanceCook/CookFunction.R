library(car)
library(gamlss)
library(ggplot2)

cook<-function(modelo){
  W <- diag(rep(1, modelo$noObs))
  
  X <- model.matrix(modelo)
  
  tX <- t(X)
  
  inverse<-solve(tX%*%W%*%X)
  
  H<-W^{1/2}%*%X%*%inverse%*%tX%*%W^{1/2}
  dgH<-diag(H)
  print(dgH)
  
  RP = diag((modelo$y - (modelo$mu.fv))/(sqrt(modelo$mu.fv) ^2))
  dgRP<-diag(RP)
  #Ingrese un abs para probar nada mas
  refactor<-abs(dgRP)
  print(dgRP)
  
  #LD<-(dgH/(1-dgH))
  LD<-(dgH/(1-dgH))*(refactor/sqrt(1-dgH))
  print(LD)
  
  datos <- data.frame(
    Observaciones = 1:modelo$noObs,
    Cooks = LD)
  indices_puntos_altos <- tail(order(datos$Cooks), 6)
  grafico <- ggplot(datos, aes(x = Observaciones, y = Cooks)) +
    geom_point(shape = 19, size = 2)+ 
    geom_text(data = datos[indices_puntos_altos, ], aes(label = Observaciones), vjust =0 ,hjust=-0.5) + 
    labs(x = "Observaciones", y = "Poon", title = "Curvatura B_d")+
    theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank())
  print(grafico)

}



