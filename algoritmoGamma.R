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

# number of iterations
iterations <- 5

# values of percentages
values <- data.frame(Iteration = 1:iterations, Percentage = rep(NA, iterations))

# Iterar sobre el número de iteraciones
for (i in 1:iterations) {
  #-------------INICIO SIMULACION
  #COVARIABLE
  x1 = rnorm(30, mean = 2, sd = 1)
  
  #ETA
  eta = 5-(4*x1)
  
  #RESPUESTA
  y = rGA(30, mu = exp(eta), sigma = 1 /sqrt(5))
  
  #DB CON DATOS SIMULADOS
  db = data.frame(y,x1)
  #attach(db)
  
  #ALTERACION
  alteracion<-2*mean(y)
  alteracion
  
  #OBTENER LOS DATOS A EXTRAER ALEATORIAMENTE DE LA DB PARA ALTERARLOS
  datosAleatorios <- sample(1:30, 5, replace = FALSE)
  
  #BUSQUEDA DE LOS DATOS EN LA DB
  dbBusqueda<-db[datosAleatorios,]
  dbAlterados <- db[datosAleatorios,]+alteracion
  
  #Obtener el numero de las observaciones en un vector
  obsVector <- as.numeric(row.names(dbBusqueda))
  print(obsVector)
  
  #alteracion solo a respuesta
  db[datosAleatorios,c("y")]<-dbAlterados[,1]
  
  #alteracion a covariable -> db[datosAleatorios,c("x1")]<-dbAlterados[,2]
  
  #alteracion a las dos variables -> db[datosAleatorios,c("y","x1")]<-dbAlterados[,c(1,2)]
  
  #MODELO
  modelGamlss = gamlss(y ~ x1,family = GA(mu.link = 'log', sigma.link = "identity"),data = db)
  
  #llamado a inflocal
  response<-infLocal(modelGamlss,"BP",5)
  compareVectors <- obsVector %in% response
  countTrue <- sum(compareVectors)
  value <- (countTrue / length(obsVector)) * 100
  
  #llamado a cook
  #cook(modelGamlss,4)
  values$Percentage[i] <- value
}
# average for porcentages of algorithm
average <- mean(values$Percentage, na.rm = TRUE)

# add value of the data
values[nrow(values) + 1, ] <- c("Detection rate",paste(average," %"))
