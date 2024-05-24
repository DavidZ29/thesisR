#Carga de funcion inflocal
source("(F)CurvaturaIteraciones.R")

#i:iteraciones
#n:numero de datos
#p:porcentaje

resultado_total <- data.frame(iteration = integer(),
                              dataNumber = integer(),
                              modification = integer(),
                              detection = integer())

iterationPoisson <- function(i, n, p) {
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
      x1<-rnorm(n,mean=2,sd=1)
      
      # ETA (b0=-6, b1=2)
      eta<- -6+(2*x1)
      
      # RESPUESTA
      y <- rPO(n,mu= exp(eta))
      
      # DB CON DATOS SIMULADOS
      db <- data.frame(y, x1)
      
      # ALTERACION
      #alteracion <- ceiling(3 * mean(db$y))
      alteracion <- 3 * mean(db$x1)
      
      
      cantidadObs <- round((p / 100) * n)
      
      # OBTENER LOS DATOS A EXTRAER ALEATORIAMENTE DE LA DB PARA ALTERARLOS
      datosAleatorios <- sample(1:n, cantidadObs, replace = FALSE)
      
      # BUSQUEDA DE LOS DATOS EN LA DB
      dbBusqueda <- db[datosAleatorios, ]
      dbAlterados <- db[datosAleatorios, ] + alteracion
      
      # Obtener el número de las observaciones en un vector
      obsVector <- as.numeric(row.names(dbBusqueda))
      
      # Alteración solo a respuesta -> db[datosAleatorios, c("y")] <- dbAlterados[, 1]
      
      # Alteración a covariable -> 
      db[datosAleatorios, c("x1")] <- dbAlterados[, 2]
      
      # Alteración a las dos variables -> db[datosAleatorios, c("y", "x1")] <- dbAlterados[, c(1, 2)]
      
      # MODELO
      modelGamlss <-
        gamlss(
          y ~ x1,
          family = PO(mu.link='log'),
          data = db
        )
      
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
    valores[nrow(valores) + 1,] <-
      c("Tasa deteccion", paste(porcentaje, " %"))
    
    dataResponse <- data.frame(iteration = i,
                               dataNumber = n,
                               modification = p,
                               detection = porcentaje)
    
    # Agregar los resultados a la tabla global
    resultado_total <<- rbind(resultado_total, dataResponse)

  } else {
    cat("Error: El valor ingresado no es válido. Debe ser 2, 5, o 10.\n")
  }
}

run<-TRUE

#Prueba profe (alterar covariable y 3 veces la medai de x1)
if(run){
  iterations<-1000
  dataNumber<-50
  iterationPoisson(iterations,dataNumber,2)
  iterationPoisson(iterations,dataNumber,5)
  iterationPoisson(iterations,dataNumber,10)
  
  #filename <- paste(sprintf("./GMSimulationsV2./SG_I%d.html", iterations))
  filename2 <- paste(sprintf("./SimulationsTest./(3x)PoissonTest_I%d.txt", iterations))
  
  #print(xtable(resultado_total), type = "html", file = filename, include.rownames = FALSE)
  write.table(resultado_total, file =filename2, row.names = FALSE)
}


if(run){
  iterations<-50
  
  dataNumber1<-50
  dataNumber2<-100
  dataNumber3<-200
  dataNumber4<-1000
  
  p2<-2
  p5<-5
  p10<-10
  
  iterationPoisson(iterations,dataNumber1,p2)
  iterationPoisson(iterations,dataNumber1,p5)
  iterationPoisson(iterations,dataNumber1,p10)
  
  iterationPoisson(iterations,dataNumber2,p2)
  iterationPoisson(iterations,dataNumber2,p5)
  iterationPoisson(iterations,dataNumber2,p10)
  
  iterationPoisson(iterations,dataNumber3,p2)
  iterationPoisson(iterations,dataNumber3,p5)
  iterationPoisson(iterations,dataNumber3,p10)
  
  iterationPoisson(iterations,dataNumber4,p2)
  iterationPoisson(iterations,dataNumber4,p5)
  iterationPoisson(iterations,dataNumber4,p10)
  
  #filename <- paste(sprintf("./GMSimulationsV2./SG_I%d.html", iterations))
  filename2 <- paste(sprintf("./POSimulationsV2/PO_I%d.txt", iterations))
  
  #print(xtable(resultado_total), type = "html", file = filename, include.rownames = FALSE)
  write.table(resultado_total, file =filename2, row.names = FALSE)
  print(paste("[ITERACIONES] ",iterations))
}
