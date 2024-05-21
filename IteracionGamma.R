#Carga de funcion inflocal
source("(F)CurvaturaIteraciones.R")

#i:iteraciones
#n:numero de datos
#p:porcentaje

resultado_total <- data.frame(iteration = integer(),
                              dataNumber = integer(),
                              modification = integer(),
                              detection = integer())

iterationGamma <- function(i, n, p) {
  # number of iterations
  iterations <- i
  
  # db de porcentajes
  valores <-
    data.frame(Iteraciones = 1:iterations,
               Porcentaje = rep(NA, iterations))
  # Validar el valor ingresado
  if (p %in% c(2, 5, 10)) {
    for (i in 1:iterations) {
      #-------------INICIO SIMULACION
      # Cantidad de observaciones
      # COVARIABLE
      x1 <- rnorm(n, mean = 2, sd = 1)
      
      # ETA (b0=5, b1=-4)
      eta <- 5 - (4 * x1)
      
      # RESPUESTA
      y <- rGA(n, mu = exp(eta), sigma = sqrt(5) / 5)
      
      # DB CON DATOS SIMULADOS
      db <- data.frame(y, x1)
      
      # ALTERACION
      alteracion <- 3 * mean(db$y)
      
      cantidadObs <- round((p / 100) * n)
      
      # OBTENER LOS DATOS A EXTRAER ALEATORIAMENTE DE LA DB PARA ALTERARLOS
      datosAleatorios <- sample(1:n, cantidadObs, replace = FALSE)
      
      # BUSQUEDA DE LOS DATOS EN LA DB
      dbBusqueda <- db[datosAleatorios, ]
      dbAlterados <- db[datosAleatorios, ] + alteracion
      
      # Obtener el número de las observaciones en un vector
      obsVector <- as.numeric(row.names(dbBusqueda))
      
      # Alteración solo a respuesta
      db[datosAleatorios, c("y")] <- dbAlterados[, 1]
      
      # Alteración a covariable -> db[datosAleatorios, c("x1")] <- dbAlterados[, 2]
      # Alteración a las dos variables -> db[datosAleatorios, c("y", "x1")] <- dbAlterados[, c(1, 2)]
      
      # MODELO
      modelGamlss <-
        gamlss(
          y ~ x1,
          family = GA(mu.link = 'log', sigma.link = "identity"),
          data = db
        )
      
      # Llamado a inflocal
      response <- infLocal(modelGamlss, "BP", 5)

      compareVectors <- obsVector %in% response
      countTrue <- sum(compareVectors)
      value <- round((countTrue / length(obsVector)) * 100)
      
      valores$Porcentaje[i] <- value
    }
    
    # Promedio de los porcentajes del algoritmo
    average <- mean(valores$Porcentaje, na.rm = TRUE)
    porcentaje <- round(average)
    
    # Agregar el valor de la tasa de detección al data frame 'valores'
    valores[nrow(valores) + 1,] <-
      c("Tasa deteccion", paste(porcentaje, " %"))
    
    dataResponse <- data.frame(iteration = i,
                               dataNumber = n,
                               modification = p,
                               detection = porcentaje)
    
    # Agregar los resultados a la tabla global
    resultado_total <<- rbind(resultado_total, dataResponse)
    
    
    dataResponse <- data.frame(iteration = i,
                               dataNumber = n,
                               modification = p,
                               detection = porcentaje)
    
    
    } else {
    cat("Error: El valor ingresado no es válido. Debe ser 2, 5, o 10.\n")
  }
}

run<-TRUE

if(run){
  iterations<-1000
  
  dataNumber1<-50
  dataNumber2<-100
  dataNumber3<-200
  dataNumber4<-1000
  
  p2<-2
  p5<-5
  p10<-10
  
  iterationGamma(iterations,dataNumber1,p2)
  iterationGamma(iterations,dataNumber1,p5)
  iterationGamma(iterations,dataNumber1,p10)

  iterationGamma(iterations,dataNumber2,p2)
  iterationGamma(iterations,dataNumber2,p5)
  iterationGamma(iterations,dataNumber2,p10)
  
  iterationGamma(iterations,dataNumber3,p2)
  iterationGamma(iterations,dataNumber3,p5)
  iterationGamma(iterations,dataNumber3,p10)
  
  iterationGamma(iterations,dataNumber4,p2)
  iterationGamma(iterations,dataNumber4,p5)
  iterationGamma(iterations,dataNumber4,p10)
  
  #filename <- paste(sprintf("./GMSimulationsV2./SG_I%d.html", iterations))
  filename2 <- paste(sprintf("./GMSimulationsV2./SG_I%d.txt", iterations))
  
  #print(xtable(resultado_total), type = "html", file = filename, include.rownames = FALSE)
  write.table(resultado_total, file =filename2, row.names = FALSE)
  print(paste("[ITERACIONES] ",iterations))
  }




