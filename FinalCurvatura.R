#---CURVATURA---#
#-librerias necesarias-#
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

#-importacion de las bases de datos-#
#-salario Gamma-#
datosTXT1 <- read.table(file.choose(), header = TRUE)

#-Lumber company Poisson-#
datosTXT2 <- read.table(file.choose(), header = TRUE)

#-modelos-#
#-Salario(GAMMA)-#
modelo1 <-
  gamlss(
    salario ~ genero + posicion + experiencia,
    data = datosTXT1,
    family = GA(mu.link = "log")
  )
#-Lumber company(POISSON)-#
modelo2 <-
  gamlss(
    nClientes ~ nViviendas + ingresoPd + edadPd + dCompetidor + dTienda,
    data = datosTXT2,
    family = PO(mu.link = "log")
  )

#-Funcion de curvatura para modelos lineales generalizados-#
infLocal <-
  function(modeloGamlss,
           parametro = NULL,
           observaciones = NULL) {
    #-logica para etiqueta de puntos-#
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
    
    #-componentes-#
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
    
    #-Logica de eleccion de distribucion y sus componentes
    #-fe1:primera derivada de funcion exponencial respecto a phi
    #-c2:segunda derivada de c
    #-V:funcion varianza
    #-W:matriz de pesos
    
    #-distribucion normal-#
    if (modeloGamlss$family[1] == "NO") {
      phi <- 1 / (modeloGamlss$sigma.fv) ^ 2
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
    
    #-distribucion inversa normal-#
    if (modeloGamlss$family[1] == "IG") {
      phi <- 1 / (modeloGamlss$sigma.fv) ^ 2
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
    
    #-distribucion poisson-#
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
    
    #-distribucion gamma-#
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
    
    #-componentes de fisher esperada-#
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
    
    #-logica para eleccion de parametros-#
    response<-NULL
    esPoisson <- modeloGamlss$family[1] == "PO"
    if (esPoisson) {
      inverselBB <- solve(lBB)
      expectedFisher = inverselBB
    }
    if ((is.null(parametro) || parametro == "B") && esPoisson) {
      response<-print("Seleccionado parametro β para Poisson")
    }
    else if ((is.null(parametro) ||
              parametro == "BP") && !esPoisson) {
      expectedFisher = inverseEF
      response<-print("Seleccionado parametros β y ϕ")
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
      response<-print("Seleccionado parametro β")
    }
    else if (parametro == "P" && !esPoisson) {
      l11 <- solve(lBB)
      L11 <- rbind(cbind(l11, lBP), cbind(lPB, 0))
      expectedFisher = inverseEF - L11
      response<-print("seleccionado parametro ϕ")
    } else if (esPoisson) {
      cat(
        "ERROR! : Parametro equivocado:",
        parametro,
        "\nLa distribucion seleccionada es Poisson: family =",
        modeloGamlss$family[1],"\n"
      )
      stop("se detuvo el proceso")
    }
    else{
      print("Error en funcion infLocal<-function(modeloGamlss,parametro)")
      stop()
    }
    
    #-residuos de pearson-#
    rP = ((modeloGamlss$y - (modeloGamlss$mu.fv)) / (sqrt(modeloGamlss$mu.fv) ^
                                                       2))
    
    #-delta bajo el primer esquema de perturbacion-#
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
    
    #grafico distancia de Cook
    if(parametro == "B" && !esPoisson){
      inverse <- solve(tX %*% W %*% X)
      H <- W ^ {
        1 / 2
      } %*% X %*% inverse %*% tX %*% W ^ {
        1 / 2
      }
      dgH <- diag(H)
      LD <- (dgH / (1 - dgH)) * (rP / sqrt(1 - dgH)) ^ {
        2
      } * (1 / modeloGamlss$sigma.fv)
      datosCook <- data.frame(Observaciones = 1:modeloGamlss$noObs,
                              Cooks =LD)
      indicesPuntos <- tail(order(datosCook$Cooks), nObservaciones)
      graficoCook <-
        ggplot(datosCook, aes(x = Observaciones, y = Cooks)) +
        geom_segment(
          data = datosCook[indicesPuntos, ],
          aes(
            x = Observaciones,
            y = 0,
            xend = Observaciones,
            yend = Cooks
          ),
          linetype = "dashed",
          color = "grey"
        ) +
        geom_point(shape = 19, size = 1.5) +
        geom_text(
          data = datosCook[indicesPuntos, ],
          aes(label = Observaciones),
          size = 3 ,
          vjust = 0 ,
          hjust = -0.5
        ) +
        labs(x = "Observaciones", y = "Distancia de Cook", title = "Grafico distancia de Cook",subtitle = response) +
        theme(panel.background = element_rect(fill = NA, color = "black"),
              panel.grid = element_blank())
      print(graficoCook)
    }
    
    #-datos de curvatura-#
    B_d = -(tdelta %*% expectedFisher %*% delta) * (sqrt(sum(
      diag(tdelta %*% expectedFisher %*% delta) ^ {
        2
      }
    ))) ^ {
      -1
    }
    poonpoon = diag(B_d)
    
    #umbral
    umbral <- 2 * mean(poonpoon)
    #umbral2<-umbral+(3*sd(poonpoon))
    
    datosCurvatura <- data.frame(Observaciones = 1:modeloGamlss$noObs,
                                 Poon = poonpoon)
    indices_puntos_altos <- tail(order(datosCurvatura$Poon), nObservaciones)

    #grafico curvatura
    grafico <- ggplot(datosCurvatura, aes(x = Observaciones, y = Poon)) +
      geom_point(shape = 19, size = 1.5) +
      geom_text(
        data = datosCurvatura[indices_puntos_altos, ],
        aes(label = Observaciones),
        size = 3 ,
        vjust = 0 ,
        hjust = -0.5
      ) +
      labs(x = "Observaciones", y = "Poon", title = "Grafico de curvatura ",subtitle = response) +
      theme(panel.background = element_rect(fill = NA, color = "black"),
            panel.grid = element_blank()) +
      geom_hline(yintercept = umbral,
                 linetype = "dashed",
                 color = "red")
    print(grafico)
  }

#-fin de funcion-#

#Para utilizar la funcion de curvatura debe llamar a la funcion 'infLocal'

#REQUERIDO: Colocar el modelo lineal generalizado
#OPCIONAL: Colocar un parametro ("B","P","BP")
#OPCIONAL: Colocar un numero (cantidad de puntos influentes etiquetados en el grafico)

#EJEMPLOS
#infLOcal(miModelo)
#infLOcal(miModelo,"B")
#infLOcal(miModelo,"P",5)

#Gama
infLocal(modelo1, "BP", 5)
infLocal(modelo1, "B", 5)
infLocal(modelo1, "P", 5)

#Poisson
infLocal(modelo2, "P", 5)
infLocal(modelo2, "B", 5)
infLocal(modelo2, "BP", 5)
