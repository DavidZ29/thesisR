#-----------------------------------------------------------------------------#
#---CURVATURA PARA ITERACIONES---#
#-----------------------------------------------------------------------------#
#-Librerias necesarias-#
#-----------------------------------------------------------------------------#
paquetesNecesarios <- c("gamlss", "ggplot2")
cargarPaquetes <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete)
    }
    library(paquete, character.only = TRUE)
  }
}
cargarPaquetes(paquetesNecesarios)

#-----------------------------------------------------------------------------#
#-Funcion de curvatura para modelos lineales generalizados-#
#-----------------------------------------------------------------------------#
infLocal <-
  function(modeloGamlss,
           parametro = NULL,
           observaciones = NULL) {
    #-------------------------------------------------------------------------#
    #-logica para etiqueta de puntos-#
    #-------------------------------------------------------------------------#
    limites <- modeloGamlss$noObs
    if (is.null(observaciones)) {
      nObservaciones <- 0
    } else if (observaciones > limites) {
      stop("EL valor digitado excede el numero de datos")
    } else if (is.numeric(observaciones) &&
               as.integer(observaciones)) {
      nObservaciones <- observaciones
    } else{
      stop("--Error, valor no entero o numerico--")
    }
    #-------------------------------------------------------------------------#
    #-componentes-#
    #-------------------------------------------------------------------------#
    #Matriz modelo
    X <- model.matrix(modeloGamlss)
    #traspuesta de la matriz modelo
    tX <- t(X)
    #valores estimados de los parametros beta
    beta <- coef(modeloGamlss)
    #variable respuesta
    y = as.vector(modeloGamlss$y)
    # valores estimados mu
    mu = as.vector(fitted(modeloGamlss, "mu"))
    #-------------------------------------------------------------------------#
    #-Logica de eleccion de distribucion y sus componentes-#
    #-fe1:primera derivada de funcion exponencial respecto a phi-#
    #-c2:segunda derivada de c-#
    #-V:funcion varianza-#
    #-W:matriz de pesos-#
    #-------------------------------------------------------------------------#
    #-distribucion normal-#
    #-------------------------------------------------------------------------#
    if (modeloGamlss$family[1] == "NO") {
      phi <- 1 / (modeloGamlss$sigma.fv[1]) ^ 2
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
      V <- as.matrix(diag(rep(1, modeloGamlss$noObs)))
      if (modeloGamlss$mu.link == "log") {
        W <- as.matrix(diag(mu ^ {
          2
        }))
      }
      if (modeloGamlss$mu.link == "identity")
      {
        W <- V
      }
      if (modeloGamlss$mu.link == "sqrt")
      {
        W <- as.matrix(diag(4 * mu))
      }
    }
    #-------------------------------------------------------------------------#
    #-distribucion inversa normal-#
    #------------------------------------------------------------------------#
    if (modeloGamlss$family[1] == "IG") {
      phi <- 1 / (modeloGamlss$sigma.fv[1]) ^ 2
      fe1 <- ((-y * mu + 2 * mu ^ {
        2
      }) / (2 * mu ^ {
        3
      })) + ((2 - y * phi) / 2 * phi)
      c2 <- -1 / (2 * phi ^ {
        2
      })
      V <- as.matrix(diag(mu ^ (3)))
      if (modeloGamlss$mu.link == "log") {
        W <- as.matrix(diag(1 * (mu) ^ (-1)))
      }
      if (modeloGamlss$mu.link == "identity")
      {
        W <- as.matrix(diag(1 * (mu) ^ (-3)))
      }
      if (modeloGamlss$mu.link == "sqrt")
      {
        W <- as.matrix(diag(4 * (mu) ^ (-2)))
      }
    }
    #-------------------------------------------------------------------------#
    #-distribucion poisson-#
    #-------------------------------------------------------------------------#
    if (modeloGamlss$family[1] == "PO") {
      phi <- 1
      fe1 <- vector("numeric", length = modeloGamlss$noObs)
      c2 <- 0
      V <- as.matrix(diag(mu))
      if (modeloGamlss$mu.link == "log") {
        W <- V
      }
      if (modeloGamlss$mu.link == "identity")
      {
        W <- as.matrix(diag(mu ^ {
          -1
        }))
      }
      if (modeloGamlss$mu.link == "sqrt")
      {
        W <- as.matrix(diag(rep(4, modeloGamlss$noObs)))
      }
    }
    #-------------------------------------------------------------------------#
    #-distribucion gamma-#
    #-------------------------------------------------------------------------#
    if (modeloGamlss$family[1] == "GA") {
      phi <- 1 / (modeloGamlss$sigma.fv[1]) ^ 2
      fe1 <- ((-y * mu ^ {
        -1
      }) - log(mu)) - digamma(phi) + log(phi * y) + 1
      c2 = 1 / phi - trigamma(phi)
      V <- diag((modeloGamlss$mu.fv) ^ 2)
      if (modeloGamlss$mu.link == "log")
      {
        W <- diag(rep(1, modeloGamlss$noObs))
      }
      if (modeloGamlss$mu.link == "identity")
      {
        W <- solve(V)
      }
      if (modeloGamlss$mu.link == "inverse")
      {
        W <- V
      }
    }
    #-------------------------------------------------------------------------#
    #-componentes de fisher esperada-#
    #------------------------------------------------------------------------#
    lBB <- -phi * tX %*% W %*% X
    lBP <- vector("numeric", length = length(beta))
    lPB <- t(lBP)
    if (modeloGamlss$family[1] == "IG") {
      lPP <- sum(unlist(c2))
    } else{
      lPP <- modeloGamlss$noObs * c2
    }
    expFisher = rbind(cbind(lBB, lBP), cbind(lPB, lPP))
    if (modeloGamlss$family[1] != "PO") {
      inverseEF <- solve(expFisher)
    }
    #-------------------------------------------------------------------------#
    #-logica para eleccion de parametros-#
    #------------------------------------------------------------------------#
    response <- NULL
    esPoisson <- modeloGamlss$family[1] == "PO"
    if (esPoisson) {
      inverselBB <- solve(lBB)
      expectedFisher = inverselBB
    }
    if ((is.null(parametro) || parametro == "B") && esPoisson) {
      response <- "Seleccionado parametro β para Poisson"
    }
    else if ((is.null(parametro) ||
              parametro == "BP") && !esPoisson) {
      expectedFisher = inverseEF
      response <- "Seleccionado parametros β y ϕ"
    }
    else if (parametro == "B" && !esPoisson) {
      mZeros <- matrix(0, nrow = length(beta), ncol = length(beta))
      if (lPP == 0) {
        l22 <- 0
      } else{
        l22 <- solve(lPP)
      }
      L22 <- rbind(cbind(mZeros, lBP), cbind(lPB, l22))
      expectedFisher = inverseEF - L22
      response <- "Seleccionado parametro β"
    }
    else if (parametro == "P" && !esPoisson) {
      l11 <- solve(lBB)
      L11 <- rbind(cbind(l11, lBP), cbind(lPB, 0))
      expectedFisher = inverseEF - L11
      response <- "seleccionado parametro ϕ"
    } else if (esPoisson) {
      cat(
        "ERROR! : Parametro equivocado:",
        parametro,
        "\nLa distribucion seleccionada es Poisson: family =",
        modeloGamlss$family[1],
        "\n"
      )
      stop("se detuvo el proceso")
    }
    else{
      print("Error en funcion *infLocal<-function(modeloGamlss,parametro)*")
      stop()
    }
    
    #-residuos de pearson-#
    rP = ((modeloGamlss$y - (modeloGamlss$mu.fv)) / (sqrt(modeloGamlss$mu.fv ^
                                                            2)))
    #------------------------------------------------------------------------#
    #-delta bajo el primer esquema de perturbacion-#
    #------------------------------------------------------------------------#
    lBW <- phi * tX %*% W ^ {
      1 / 2
    } %*% diag(rP)
    if (modeloGamlss$family[1] == "PO") {
      delta <- lBW
      tdelta <- t(delta)
    } else{
      lPW <- t(unlist(fe1))
      delta <- rbind(lBW, lPW)
      tdelta <- t(delta)
    }
    #------------------------------------------------------------------------#
    #-curvatura y vector deteccion-#
    #------------------------------------------------------------------------#
    #-datos-#
    B_d = -(tdelta %*% expectedFisher %*% delta) * (sqrt(sum(
      diag(tdelta %*% expectedFisher %*% delta) ^ {
        2
      }
    ))) ^ {
      -1
    }
    poonpoon = diag(B_d)
    
    #DB
    datosCurvatura <-
      data.frame(Observaciones = 1:modeloGamlss$noObs,
                 Poon = poonpoon)
    
    #obtener 5% de datos
    datosOrdenados <- datosCurvatura[order(-datosCurvatura$Poon), ]
    nPorciento <- round(0.05 * nrow(datosCurvatura))
    
    indices_puntos_altos <-
      tail(order(datosCurvatura$Poon), nPorciento)
    
    vectorDeteccion <- as.vector(row.names(datosOrdenados[1:nPorciento, ]))
    
    print(response)
    #retorna vector deteccion
    return(vectorDeteccion)
  }
#-----------------------------------------------------------------------------#
#-fin de funcion-#
#-----------------------------------------------------------------------------#