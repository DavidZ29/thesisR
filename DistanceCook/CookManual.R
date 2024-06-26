install.packages("car")
library(car)
library(gamlss)
library(ggplot2)

#Gamma
dataS<-read.table(file.choose(),header = TRUE)

#GLM
modelGlm<-glm(salario ~genero+ posicion + experiencia, data = dataS,family = Gamma(link = "log"))
summary(modelGlm)

#GAMLSS
modelGamlss <-gamlss(salario ~ genero + posicion + experiencia,data = dataS,family = GA(mu.link = "log"))
summary(modelGamlss)

W <- diag(rep(1, modelGamlss$noObs))


############
X <- model.matrix(modelGamlss)
tX <- t(X)

inverse<-solve(tX%*%W%*%X)

H<-W^{1/2}%*%X%*%inverse%*%tX%*%W^{1/2}
dgH<-diag(H)
print(dgH)


RP = ((modelGamlss$y - (modelGamlss$mu.fv))/(sqrt(modelGamlss$mu.fv) ^2))
#dgRP<-diag(RP)
#Ingrese un abs para probar nada mas
#refactor<-abs(dgRP)
#print(dgRP)
refactor=RP
#LD<-(dgH/(1-dgH))
LD<-(dgH/(1-dgH))*(refactor/sqrt(1-dgH))^{2}*(1/modelGamlss$sigma.fv)

print(LD)

datos <- data.frame(
  Observaciones = 1:modelGamlss$noObs,
  Cooks = LD
)
indices_puntos_altos <- tail(order(datos$Cooks), 6)
grafico <- ggplot(datos, aes(x = Observaciones, y = Cooks)) +
  geom_point(shape = 19, size = 2)+ 
  geom_text(data = datos[indices_puntos_altos, ], aes(label = Observaciones), vjust =0 ,hjust=-0.5) + 
  labs(x = "Observaciones", y = "Poon", title = "Curvatura B_d")+
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank())

print(grafico)

infIndexPlot(modelGlm,var="Cook",id=list(method="y", n=6, cex=.5, col=carPalette()[1], location="lr"), grid=TRUE, main="Diagnostico")
