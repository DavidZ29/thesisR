paquetesNecesarios <- c("gamlss", "readxl", "ggplot2")
cargarPaquetes <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete)
    }
    library(paquete, character.only = TRUE)
  }
}
cargarPaquetes(paquetesNecesarios)


datosTXT1 <- read.table(file.choose(), header = TRUE)
modelo1 <-
  gamlss(
    salario ~ genero + posicion + experiencia,
    data = datosTXT1,
    family = GA(mu.link = "log")
  )


#-componentes-#
#-----------------------------------------------------------------------------#
#Matriz modelo
X <- model.matrix(modelo1)
#-----------------------------------------------------------------------------#
#traspuesta de la matriz modelo
tX <- t(X)
#-----------------------------------------------------------------------------#
#valores estimados de los parametros beta
beta <- coef(modelo1)
#-----------------------------------------------------------------------------#
#variables respuesta
y = as.vector(modelo1$y)
#-----------------------------------------------------------------------------#
# valores estimados mu
mu = as.vector(fitted(modelo1, "mu"))

mu
length(mu)
#-----------------------------------------------------------------------------#
#-Logica de eleccion de distribucion y sus componentes-#
#-fe1:primera derivada de funcion exponencial respecto a phi-#
#-c2:segunda derivada de c-#
#-V:funcion varianza-#
#-W:matriz de pesos-#
#-----------------------------------------------------------------------------#
#-distribucion normal-#
#-----------------------------------------------------------------------------#
if (modelo1$family[1] == "NO") {
  phi <- 1 / (modelo1$sigma.coefficients) ^ 2
  fe1 <- y * mu - ((1 / 2) * (mu ^ {
    2
  } + y ^ {
    2
  } - phi ^ {
    -1
  }))
  c2 <- 1 / (2 * phi ^ {
    2
  })
  V <- as.matrix(diag(rep(1, modelo1$noObs)))
  if (modelo1$mu.link == "log") {
    W <- as.matrix(diag(mu ^ {
      2
    }))
  }
  if (modelo1$mu.link == "identity")
  {
    W <- V
  }
  if (modelo1$mu.link == "sqrt")
  {
    W <- as.matrix(diag(4 * mu))
  }
}
#-----------------------------------------------------------------------------#
#-distribucion inversa normal-#
#-----------------------------------------------------------------------------#
if (modelo1$family[1] == "IG") {
  phi <- 1 / (modelo1$sigma.coefficients) ^ 2
  fe1 <- ((-y * mu + 2 * mu ^ {
    2
  }) / (2 * mu ^ {
    3
  })) + ((2 - y * phi) / 2 * phi)
  c2 <- -1 / (2 * phi ^ {
    2
  })
  V <- as.matrix(diag(mu ^ (3)))
  if (modelo1$mu.link == "log") {
    W <- as.matrix(diag(1 * (mu) ^ (-1)))
  }
  if (modelo1$mu.link == "identity")
  {
    W <- as.matrix(diag(1 * (mu) ^ (-3)))
  }
  if (modelo1$mu.link == "sqrt")
  {
    W <- as.matrix(diag(4 * (mu) ^ (-2)))
  }
}
#-----------------------------------------------------------------------------#
#-distribucion poisson-#
#-----------------------------------------------------------------------------#
if (modelo1$family[1] == "PO") {
  phi <- 1
  fe1 <- vector("numeric", length = modelo1$noObs)
  c2 <- 0
  V <- as.matrix(diag(mu))
  if (modelo1$mu.link == "log") {
    W <- V
  }
  if (modelo1$mu.link == "identity")
  {
    W <- as.matrix(diag(mu ^ {
      -1
    }))
  }
  if (modelo1$mu.link == "sqrt")
  {
    W <- as.matrix(diag(rep(4, modelo1$noObs)))
  }
}
#-----------------------------------------------------------------------------#
#-distribucion gamma-#
#-----------------------------------------------------------------------------#
if (modelo1$family[1] == "GA") {
  phi <- 1 / (modelo1$sigma.coefficients) ^ 2
  fe1 <- (-y * mu ^ {
    -1
  } - log(mu)) - digamma(phi) + log(phi * y) + 1
  #c2 = 1 / phi - trigamma(phi)
  c2 = 1 / phi - trigamma(phi)
  V <- diag((modelo1$mu.fv) ^ 2)
  if (modelo1$mu.link == "log")
  {
    W <- diag(rep(1, modelo1$noObs))
  }
  if (modelo1$mu.link == "identity")
  {
    W <- solve(V)
  }
  if (modelo1$mu.link == "inverse")
  {
    W <- V
  }
}
#-----------------------------------------------------------------------------#
#-componentes de fisher esperada-#
#-----------------------------------------------------------------------------#
lBB <- -phi * tX %*% W %*% X
lBP <- vector("numeric", length = length(beta))
lPB <- t(lBP)
if (modelo1$family[1] == "IG") {
  lPP <- sum(unlist(c2))
} else{
  #lPP <- -modelo1$noObs * c2
  lPP <- modelo1$noObs * c2
}
expFisher = rbind(cbind(lBB, lBP), cbind(lPB, lPP))
if (modelo1$family[1] != "PO") {
  inverseEF <- solve(expFisher)
}
#-----------------------------------------------------------------------------#
#-logica para eleccion de parametros-#
#-----------------------------------------------------------------------------#
esPoisson <- modelo1$family[1] == "PO"
if (esPoisson) {
  inverselBB <- solve(lBB)
  expectedFisher = inverselBB
}
if ((is.null(parametro) || parametro == "B") && esPoisson) {
  print("Seleccionado parametro β para Poisson")
}
else if ((is.null(parametro) || parametro == "BP") && !esPoisson) {
  expectedFisher = inverseEF
  print("Seleccionado parametros β y ϕ")
}
else if (parametro == "B" && !esPoisson) {
  mZeros <- matrix(0, nrow = length(beta), ncol = length(beta))
  if (lPP == 0) {
    l22 <- 0
}else{
    l22 <- solve(lPP)
}
  L22 <- rbind(cbind(mZeros, lBP), cbind(lPB, l22))
  expectedFisher = inverseEF - L22
  print("Seleccionado parametro β")
}
else if (parametro == "P" && !esPoisson) {
  l11 <- solve(lBB)
  L11 <- rbind(cbind(l11, lBP), cbind(lPB, 0))
  expectedFisher = inverseEF - L11
  print("seleccionado parametro ϕ")
} else if (esPoisson) {
  cat(
    "ERROR! : Parametro equivocado:",
    parametro,
    "\nLa distribucion seleccionada es Poisson: family =",
    modelo1$family[1]
  )
}
else{
  print("Error en funcion infLocal<-function(modelo1,parametro)")
}
#-----------------------------------------------------------------------------#
#-inversa de fisher esperada y residuos de pearson-#
#-----------------------------------------------------------------------------#
rP = diag((modelo1$y - (modelo1$mu.fv)) / (sqrt(modelo1$mu.fv) ^ 2))
#-----------------------------------------------------------------------------#
#-delta bajo el primer esquema de perturbacion-#
#-----------------------------------------------------------------------------#
lBW <- phi * tX %*% W ^ {
  1 / 2
} %*% rP
if (modelo1$family[1] == "PO") {
  delta <- lBW
  tdelta <- t(delta)
} else{
  lPW <- t(unlist(fe1))
  delta <- rbind(lBW, lPW)
  tdelta <- t(delta)
}
#-----------------------------------------------------------------------------#
#-curvatura y grafico-#
#-----------------------------------------------------------------------------#
B_d = -(tdelta %*% expectedFisher %*% delta) * (sqrt(sum(
  diag(tdelta %*% expectedFisher %*% delta) ^ {
    2
  }
))) ^ {
  -1
}
poonpoon = diag(B_d)

umbral <- 2 * mean(poonpoon)
umbral2 <- umbral + (3 * sd(poonpoon))

datos <- data.frame(Observaciones = 1:modelo1$noObs,
                    Poon = poonpoon)
indices_puntos_altos <- tail(order(datos$Poon), nObservaciones)

grafico <- ggplot(datos, aes(x = Observaciones, y = Poon)) +
  geom_point(shape = 19, size = 2) +
  geom_text(
    data = datos[indices_puntos_altos,],
    aes(label = Observaciones),
    vjust = 0 ,
    hjust = -0.5
  ) +
  labs(x = "Observaciones", y = "Poon", title = "Curvatura B_d") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank()) +
  geom_hline(yintercept = umbral2,
             linetype = "dashed",
             color = "red")
print(grafico)
