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
datosNormal <- function(n) {
  
  if (!is.numeric(n)) {
    stop("El dato ingresado no es numérico.")
  }
  
  if (n<=0) {
    stop("El dato ingresado es menor o igual a cero")
  }
  
  #semilla de creacion de datos alaetorios estatica
  set.seed(2901)
  
  #Covariables
  #mean determina media
  #sd determina desviación estándar 
  cov1 <- rnorm(n, mean  = 2, sd = 2)
  
  #Variable respuesta
  response <- 2 + 3 * cov1 + rnorm(n, mean = 0, sd = 1)
  
  #Data frame con los datos
  db <- data.frame(y =response , x1 = cov1)
  
  #Modelo gamlss
  model <- gamlss(y ~ x1, data = db, family = GA)
  
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
datosNormal(30)

