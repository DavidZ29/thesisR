#655 lumber
#model=gamlss(precio~metros+anyo+calefaccion+situacion,data=datos,family=GA(mu.link="inverse"))

library(gamlss)
dataSalary<-read.table(file.choose(),header = TRUE)
modelSalary<-gamlss(salario ~genero+ posicion + experiencia, data = dataSalary,family = GA(mu.link="log"))

#modajustado=model
inflocal=function(modajustado){
  X=model.matrix(modajustado)##Matriz de diseño
  beta=coef(modajustado)##valores estimados efectos fijos
  if(modajustado$family[1]=="GA"){
    phi=1/(modajustado$sigma.coefficients)^2
  V=diag((modajustado$mu.fv)^2)
  if(modajustado$mu.link=="log")
    {
  W=diag(rep(1,modajustado$noObs))
  }
  if(modajustado$mu.link=="identity")
  {
    W=solve(V)
  }
  if(modajustado$mu.link=="inverse")
  {
    W=V
  }
  }
  Rp=diag(sqrt(phi)*(modajustado$y-(modajustado$mu.fv))/(modajustado$mu.fv)^2)
  mdelta=sqrt(phi)*t(X)%*%sqrt(W)%*%Rp
  Fbeta2=phi*t(X)%*%W%*%X###Fisher esperada
  B=t(mdelta)%*%Fbeta2%*%mdelta
  poonpoon=diag(B*(sqrt(sum(diag(B%*%B))))^{-1})
  plot(1:modajustado$noObs,poonpoon,pch=19,cex=0.60)
}
inflocal(modelSalary)
