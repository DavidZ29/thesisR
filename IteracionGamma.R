#Carga de funcion inflocal
source("(F)CurvaturaIteraciones.R")

# number of iterations
iterations <- 50

# db de porcentajes
valores <- data.frame(Iteraciones = 1:iterations, Porcentaje = rep(NA, iterations))

# Iterar sobre el número de iteraciones
for (i in 1:iterations) {
  #-------------INICIO SIMULACION
  #COVARIABLE
  x1 = rnorm(30, mean = 2, sd = 1)
  
  #ETA
  eta = 5-(4*x1)
  
  #RESPUESTA
  y = rGA(30, mu = exp(eta), sigma = sqrt(5)/5)
  
  #DB CON DATOS SIMULADOS
  db = data.frame(y,x1)
  #attach(db)
  
  #ALTERACION
  alteracion<-3*mean(db$y)
  
  #OBTENER LOS DATOS A EXTRAER ALEATORIAMENTE DE LA DB PARA ALTERARLOS
  datosAleatorios <- sample(1:30, 2, replace = FALSE)
  
  #BUSQUEDA DE LOS DATOS EN LA DB
  dbBusqueda<-db[datosAleatorios,]
  dbAlterados <- db[datosAleatorios,]+alteracion
  
  #Obtener el numero de las observaciones en un vector
  obsVector <- as.numeric(row.names(dbBusqueda))
  
  #alteracion solo a respuesta
  db[datosAleatorios,c("y")]<-dbAlterados[,1]
  
  #alteracion a covariable -> db[datosAleatorios,c("x1")]<-dbAlterados[,2]
  #alteracion a las dos variables -> db[datosAleatorios,c("y","x1")]<-dbAlterados[,c(1,2)]
  
  #MODELO
  modelGamlss = gamlss(y ~ x1,family = GA(mu.link = 'log', sigma.link = "identity"),data = db)
  
  #llamado a inflocal
  response<-infLocal(modelGamlss,"P")
  
  compareVectors <- obsVector %in% response
  countTrue <- sum(compareVectors)
  value <- round((countTrue / length(obsVector)) * 100)
  
  valores$Porcentaje[i] <- value
}
# average for porcentages of algorithm
average <-mean(valores$Porcentaje, na.rm = TRUE)
porcentaje<-round(average)
# add value of the data
valores[nrow(valores) + 1, ] <- c("Tasa deteccion",paste(porcentaje," %"))
