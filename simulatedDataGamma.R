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
  
  #Covariables
  #betas
  b0=7
  b1=5
  n=100
  
  #rate = 1/scale
  #cov1 <- rgamma(n, shape = 2, scale = 2)
  #varianza sigma^2
  
  cov1<-rnorm(n,mean=100,sd=4)

  #eta
  eta <- b0 + (b1 * cov1)
  
  mu<-exp(eta)
  
  #shape determina la forma 
  #scale determina la escala
  #respuesta
  respuesta <- rgamma(n, shape = mu/2, scale = 2)
  
  #Data frame con los datos
  db <- data.frame(y =respuesta , x1 = cov1)
  
  #Modelo gamlss
  model <- gamlss(y ~ x1, data = db, family = GA(mu.link = "log"))
  
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

