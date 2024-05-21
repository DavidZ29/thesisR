#Carga de funcion inflocal
source("(F)CurvaturaIteraciones.R")
options(warn = -1)

# number of iterations
iterations <- 1000

# db de porcentajes
valores <- data.frame(Iteraciones = 1:iterations, Porcentaje = rep(NA, iterations))

# Solicitar valor al usuario
valor <- as.numeric(readline(prompt = "Ingrese un valor [2, 5, 10] -> "))

# Validar el valor ingresado
if (valor %in% c(2, 5, 10)) {
  for (i in 1:iterations) {
    #-------------INICIO SIMULACION
    # Cantidad de observaciones
    n <- 1000
    # COVARIABLE
    x1<-rnorm(n,mean=2,sd=1)
    
    # ETA (b0=5, b1=-4)
    eta<- -6+(2*x1)
    
    # RESPUESTA
    y <- rPO(n,mu= exp(eta))
    
    # DB CON DATOS SIMULADOS
    db <- data.frame(y, x1)
    
    # ALTERACION
    alteracion <- round(3 * mean(db$y))
    
    cantidadObs<-round((valor/100) * n)
    
   
    # OBTENER LOS DATOS A EXTRAER ALEATORIAMENTE DE LA DB PARA ALTERARLOS
    datosAleatorios <- sample(1:n, cantidadObs, replace = FALSE)
    
    # BUSQUEDA DE LOS DATOS EN LA DB
    dbBusqueda <- db[datosAleatorios,]
    dbAlterados <- db[datosAleatorios,] + alteracion
    
    # Obtener el número de las observaciones en un vector
    obsVector <- as.numeric(row.names(dbBusqueda))
    
    # Alteración solo a respuesta
    db[datosAleatorios, c("y")] <- dbAlterados[, 1]
    
    # Redondear la columna y y convertirla a enteros
    #db$y <- as.integer(round(db$y))
    
    # Alteración a covariable -> db[datosAleatorios, c("x1")] <- dbAlterados[, 2]
    # Alteración a las dos variables -> db[datosAleatorios, c("y", "x1")] <- dbAlterados[, c(1, 2)]
    # MODELO
    modelGamlss <- gamlss(y ~ x1, family = PO(mu.link='log'), data = db)
    # Llamado a inflocal
    response <- infLocal(modelGamlss, "B", 5)
    
    compareVectors <- obsVector %in% response
    countTrue <- sum(compareVectors)
    value <- round((countTrue / length(obsVector)) * 100)
    
    valores$Porcentaje[i] <- value
  }
  
  # Promedio de los porcentajes del algoritmo
  average <- mean(valores$Porcentaje, na.rm = TRUE)
  porcentaje <- round(average)
  
  # Agregar el valor de la tasa de detección al data frame 'valores'
  valores[nrow(valores) + 1, ] <- c("Tasa deteccion", paste(porcentaje, " %"))
  
  # nombre del archivo
  nombre_archivo <- readline(prompt = "Ingrese el nombre del archivo Excel (e.g., 'valores.xlsx') -> ")
  
  #  ruta y nombre del archivo
  ruta_archivo <- file.path("./POexcelSimulations", nombre_archivo)
  
  # guardar el excel
  write_xlsx(valores, path = ruta_archivo)
  
  cat("Archivo Excel guardado con éxito en:", ruta_archivo, "\n")
} else {
  cat("Error: El valor ingresado no es válido. Debe ser 2, 5, o 10.\n")
}
