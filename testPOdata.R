##generación de un conjunto de datos con
##respuesta Poisson


x1=rnorm(50,mean=2,sd=1)
eta=5-(4*x1)
#Generate the dependent variable
require(gamlss)

y3=rPO(50,mu= exp(eta))

datospo=data.frame(y3,x1)
attach(datospo)
#create the model
alteracion <- 3 * mean(datospo$y3)
alteracion
m3=gamlss(y3~x1,family=PO(mu.link='log'),
          data=datospo)
summary(m3)
plot(m3)
