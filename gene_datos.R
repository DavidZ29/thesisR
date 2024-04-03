##generación de un conjunto de datos con
##respuesta Poisson
#set.seed(1010)
x1=rnorm(100,2,1)
#set.seed(1010)
x2=rnorm(100,1,1)
log.mean=5-(4*x1)+(2.5*x2)
#Generate the dependent variable
require(gamlss)
#set.seed(1010)
y3=rPO(100, exp(log.mean))
datospo=data.frame(x1,x2,y3)
attach(datospo)
#create the model
m33=glm(y3~x1+x2,family=poisson(link='log'),
        data=datospo)
summary(m33)
m3=gamlss(y3~x1+x2,family=PO(mu.link='log'),
          data=datospo)
summary(m3)
plot(m3)
#####generando datos gamma
#variables indemendientes
x1=rnorm(100,2,1)
#set.seed(1010)
x2=rnorm(100,1,1)
log.mean=5-(4*x1)+(2.5*x2)
#variables dependientes
require(gamlss)
#set.seed(1010)
#sqrt(5)/5=0.4472136
#phi=1/sigma^2 para gamlss
y3=rGA(100, exp(log.mean),sigma=sqrt(5)/5)
datosga=data.frame(x1,x2,y3)
attach(datosga)
datosga
#create the model
m44=glm(y3~x1+x2,family=Gamma(link='log'),
        data=datosga)
summary(m44)
m4=gamlss(y3~x1+x2,family=GA(mu.link='log',
                             sigma.link="identity"),
          data=datosga)
summary(m4)
plot(m3)
