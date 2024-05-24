library(car)
library(gamlss)
library(ggplot2)

#Poisson lumber
datosTXT2 <- read.table(file.choose(), header = TRUE)

#----- DISTANCIA DE COOK -----#
#GAMLSS
modelGamlss <-
  gamlss(
    nClientes ~ nViviendas + ingresoPd + edadPd + dCompetidor + dTienda,
    data = datosTXT2,
    family = PO(mu.link = "log")
  )
summary(modelGamlss)

#GLM
modelGlm<-glm(nClientes ~ nViviendas + ingresoPd + edadPd + dCompetidor + dTienda,data = datosTXT2,family = poisson(link = "log"))
summary(modelGlm)

summary(modelGamlss)


observations<-10
#--------------------------------------------------
#X modelo
X <- model.matrix(modelGamlss)
#traspuesta del modelo
tX <- t(X)
#mu
mu = as.vector(fitted(modelGamlss, "mu"))
phi <- 1
V <- as.matrix(diag(mu))

#pesos
W <- V
#inversa
inverse<-solve(tX%*%W%*%X)
#H
H<-W^{1/2}%*%X%*%inverse%*%tX%*%W^{1/2}
dgH<-diag(H)

#residuos Pearson
RP = ((modelGamlss$y - (modelGamlss$mu.fv))/(sqrt(modelGamlss$mu.fv^{2})))

#LD
LD<-(dgH/(1-dgH))*(RP/sqrt(1-dgH))^{2}*phi

#LD<-(dgH/(1-dgH))*(RP/sqrt(1-dgH))^{2}*(1/modelGamlss$sigma.fv)
#datos de observaciones y distancias
datos <- data.frame(
  Observaciones = 1:modelGamlss$noObs,
  Cooks = LD
)
indicesPuntos <- tail(order(datos$Cooks), observations)
grafico <- ggplot(datos, aes(x = Observaciones, y = Cooks)) +
  geom_segment(data = datos[indicesPuntos, ], aes(x = Observaciones, y = 0, xend = Observaciones, yend = Cooks), linetype = "dashed", color = "grey")+
  geom_point(shape = 19, size = 1.5)+
  geom_text(data = datos[indicesPuntos, ], aes(label = Observaciones), size=3 ,vjust =0 ,hjust=-0.5) + 
  labs(x = "Observaciones", y = "Distancia de Cook")+
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank())
print(grafico)



#DISTANCIA DE COOK PAQUETE 
cook_distancia <- cooks.distance(modelGlm)
plot(cook_distancia, pch = 20,n=observations, main = "Distancia de Cook",
     xlab = "Número de observación", ylab = "Distancia de Cook")

#DISTANCIA DE COOK INFINDEXPLOT 
infIndexPlot(modelGlm,var="Cook",id=list(method="y", n=observations, cex=.4, col=carPalette()[1], location="lr"), grid=TRUE, main="Diagnostico")

