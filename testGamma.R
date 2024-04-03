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
  
  if (n <= 0) {
    stop("El dato ingresado es menor o igual a cero")
  }
  
  #covariables
  x1 = rnorm(n, mean = 2, sd = 1)
  
  muPredicted = 5 - (4 * x1)
  
  #phi=1/sigma^2 para gamlss -> sigma=1/sqrt(phi)
  y = rGA(n, mu = exp(muPredicted), sigma = sqrt(5) / 5)
  
  datosga = data.frame(x1, y)
  
  #create the model
  model = gamlss(y ~ x1,
                 family = GA(mu.link = 'log', sigma.link = "identity"),
                 data = datosga)
  summary(model)
  plot(model)
}

datosGamma(10)

