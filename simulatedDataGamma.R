#Automatizacion de carga de paquetes
paquetes_necesarios <- c("gamlss", "xlsx")
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
datosGamma <- function(n) {
  
  if (!is.numeric(n)) {
    stop("El dato ingresado no es numérico.")
  }
  
  if (n<=0) {
    stop("El dato ingresado es menor o igual a cero")
  }
  
  #semilla de creacion de datos alaetorios estatica
  set.seed(2901)
  
  #BETAS
  b0=2
  b1=3
  n=30
  #COVARIABLE
  cov1<-rnorm(n,mean=5,sd=4)
  cov1
  length(cov1)
  
  #COMPONENTES PARA VARIABLE RESPUESTA
  eta <- b0 + (b1 * cov1)
  muExp<-exp(eta)
  muExp
  length(muExp)
  
  #VARIABLE RESPUESTA
  respuesta<-rGA(n, mu = muExp, sigma = 4)
  respuesta
  length(respuesta)
  res<-as.numeric(respuesta)
  #Data frame con los datos
  db1 <- data.frame(y =res , x = cov1)
  
  #Modelo gamlss
  model <- gamlss(y~ x, data = db1, family = GA(mu.link = "log"))
  
  #Resumen del modelo
  print(summary(model))
  
  #Grafico plot del modelo
  plot(model)
  
  #Exportacion de datos generados
  exportExcel <- function(db) {
    respuesta <- utils::menu(c("Sí", "No"), title = "¿Desea generar el archivo Excel?")
    if (respuesta == 1) {
      write.xlsx(db, file = "datos.xlsx")
      print("Los datos se han exportado correctamente a excel en su raiz del proyecto.")
    } else {
      print("No se ha generado el archivo Excel.")
    }
  }
  exportExcel(db)
  
  }

# Llamar a la función con 30 datos simulados
datosGamma(30)

