#Carga de curvatura
source("functions.R")


paquetes_necesarios <- c("gamlss", "xlsx","ggplot2","car")
cargar_paquetes <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete)
    }
    library(paquete, character.only = TRUE)
  }
}
cargar_paquetes(paquetes_necesarios)

#datosGamma <- function(n,b0,b1) {
  
  n=100
  b0=6
  b1=-8
  set.seed(2901)
  #covariables
  x1 = rnorm(n, mean = 2, sd = 1)
  eta = b0 + (b1 * x1)
  y = rGA(n, mu = exp(eta), sigma = sqrt(5) / 5)
  
  #Notacion options(scipen = 0) or options(scipen = 999)
  
  
  db = data.frame(y,x1)
  attach(db)
  
  numeroDatos <- round(n * (5 / 100))
  alteracion<-2*mean(y)
  datosAleatorios <- sample(1:n, numeroDatos, replace = FALSE)
  dbDatosObtenido<-db[datosAleatorios, ]
  dbDatosAlterados <- db[datosAleatorios,]+alteracion
  
  db[datosAleatorios,c("y","x1")]<-dbDatosAlterados
  
  
  model = gamlss(y ~ x1,family = GA(mu.link = 'log', sigma.link = "identity"),data = db)
  #modelGlm<-glm(y ~ x1, data = db,family = Gamma(link = "log"))
  
  summary(model)
  plot(model)
  
  infLocal(model,"B",5)
  
  
  ##############---COOK DISTANCE--##############
  infIndexPlot(modelGlm,var="Cook",id=list(method="y", n=5, cex=.8, col=carPalette()[1], location="lr"), grid=TRUE, main="Diagnostico")
  
  
  #Logica de comparacion de simulacion
  #coeficientes <- model$mu.coefficients
  cof1<-abs(b0-model$mu.coefficients[1])
  cof2<-abs(b1-model$mu.coefficients[2])
  cat("Valores ingresados: Intercepto -> ",b0," x1 -> ",b1,"\nValores gamlss: Intercepto -> ",model$mu.coefficients[1]," x1 -> ",model$mu.coefficients[2],"\n")
  cat("**DIFERENCIA**\nb0",cof1,"\nb1",cof2,"\n")
  
  #Exportacion de datos generados
  exportExcel <- function(db) {
    respuesta <- utils::menu(c("Sí", "No"), title = "¿Desea generar el archivo Excel?")
    if (respuesta == 1) {
      write.xlsx(db, file = "datosSimuladosGamma.xlsx")
      print("Los datos se han exportado correctamente a excel en su raiz del proyecto.")
    } else {
      print("No se ha generado el archivo Excel.")
    }
  }
  exportExcel(db)
  print(db)
#}
datosGamma(4,6,-8)


