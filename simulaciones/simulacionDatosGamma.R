#Carga de funcion inflocal
source("functions.R")

#Automatizacion de carga de paquetes
paquetes_necesarios <- c("gamlss", "xlsx","ggplot2")
cargar_paquetes <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete)
    }
    library(paquete, character.only = TRUE)
  }
}
cargar_paquetes(paquetes_necesarios)

#Funcion simuladora de datos gamma
#Se solicita "n" (numero de datos simulados)
datosGamma <- function(n,b0,b1) {
  if (!is.numeric(n) || !is.numeric(b0) || !is.numeric(b1)) {
    stop("Algun dato ingresado no es numérico.")
  }
  if(b0==0 || b1==0){
    stop("Algun dato ingresado es cero.")
  }
  if (n <= 0) {
    stop("El dato ingresado es menor o igual a cero")
  }
  set.seed(2901)
  #covariables
  x1 = rnorm(n, mean = 2, sd = 1)
  
  eta = b0 + (b1 * x1)
  
  #phi=1/sigma^2 para gamlss -> sigma=1/sqrt(phi)
  #y = rGA(n, mu = exp(eta), sigma = sqrt(5) / 5)
  y = rGA(n, mu = exp(eta), sigma = 1 /sqrt(5))
  
  db = data.frame(y,x1)
  attach(db)
  
  #########################IMPLEMENTATION
  numeroDatos <- round(n * (5 / 100))
  alteracion<-2*mean(y)
  # Generar for con datos aleatorios
  # 
  datosAleatorios <- sample(1:n, numeroDatos, replace = FALSE)
  print(datosAleatorios)
  dbDatosObtenido<-db[datosAleatorios, ]
  print(dbDatosObtenido)
  dbDatosAlterados <- db[datosAleatorios,]+alteracion
  print(dbDatosAlterados)
  db[datosAleatorios,c("y","x1")]<-dbDatosAlterados
  ##################################################################
  
  #create the model
  model = gamlss(y ~ x1,family = GA(mu.link = 'log', sigma.link = "identity"),data = db)
  summary(model)
  plot(model)
  
  #######IMPLEMENTATION INF LOCAL
  infLocal(model,"B",numeroDatos)
  ######################
  
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
}
datosGamma(50,5,-7)
