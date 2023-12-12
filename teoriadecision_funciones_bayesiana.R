#
# PROBLEMAS DE ÁRBOLES DE DECISIÓN BAYESIANOS EN AMBIENTE DE RIESGO ------
#
## Datos de entrada ------
# n: número alternativas  (k)
# m: número estados naturaleza (i)
# q: número modalidades variable X
# Alternativas: A_1,...,A_k
# Probs. de E_i (estados) : P(E_i)
# Probs. Condic.: P(X_j|E_i)
# Tabla Valoraciones: TV(A_k,E_i)

## Se obtiene: --------
# Probs de X_j: P(X_j)
# Probs Condic.: P(E_i|X_j)  (a posteriori)
# Esperanzas a priori: E(A_k)  (a priori)
# Esperanzas a posteriori, para cada X_j: E(A_k|X_j)

# Funciones: Sintaxis ------
# func_estudio_arbolbayesiano_completo = function(probs_estados,probs_X_cond_estados,tv,
#                                                 favorable=T,baselogaritmo=2,
#                                                 nb_alt = NULL,nb_est = NULL,nb_X = NULL)


# func_estudio_entropias = function(probs_estados,probs_X_cond_estados,baselogaritmo=2,
#                                   nb_alt = NULL,nb_est = NULL,nb_X = NULL)

# func_estudio_arbolbayesiano_benefcostes = function(probs_estados,probs_X_cond_estados,tv,favorable=T,
#                                                    nb_alt = NULL,nb_est = NULL,nb_X = NULL)

# func_distribucion_conjunta = function(probs_estados,probs_X_cond_estados)
# func_distribucion_marginal_X = function(probs_estados,probs_X_cond_estados)
# func_distribucion_E_cond_X = function(probs_estados,probs_X_cond_estados)
# # General
# # A priori
# func_valoracion_esperada = function(tabla_valoraciones,probs_estados)
# func_valoracion_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,
#                                                        probs_X_cond_estados)
# func_mejor_valoracion_esperada_alternativa = function(valoracion_esperada,favorable=T)
# func_mejor_alternativa = function(valoracion_esperada,favorable=T)
# func_mejor_alternativa_multiple = function(valoracion_esperada,favorable=T)
# func_valoracion_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=T)
# func_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=T)
# func_mejor_alternativa_apriori_multiple = function(tabla_valoraciones,probs_estados,favorable=T)

# func_VEIP_tv = function(tv,probs_estados,favorable=TRUE,que_alt_Ref = NULL)

# # devuelve vector con las mejores valoraciones para cada x_j de X
# func_valoracion_mejor_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,
#                                             probs_X_cond_estados,favorable=T)
# # devuelve vector con las mejores alternativas para cada x_j de X
# func_mejor_alternativa_a_posteriori = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,
#                                                favorable=T)
# # REII: Resultado esperado con información perfecta
# func_REII = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=T)
# # RER: Resultado esperado con riesgo  (a priori)
# func_RER = function(tabla_valoraciones,probs_estados,favorable=T)
# # VEX: valor esperado máximo para el decisor por la información adicional X
# func_VEX = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=T)
# ## función de entropía de una distribución unidimensional (marginal o condicionada)
# func_entropia = function(probs,baselogaritmo=2)
# func_entropias_estados_cond_X = function(probs_estados,probs_X_cond_estados,baselogaritmo=2)
# func_entropia_aposteriori = function(probs_estados,probs_X_cond_estados,baselogaritmo=2)
# func_informacion_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2)
# func_coeficiente_redundancia_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2)

# Mejoras: -------
## 13-1-2021: -------
# - Nueva función para calcular el VEIP:
#     "func_VEIP_tv = function(tv,probs_estados,favorable=TRUE,que_alt_Ref = NULL,
#                         nb_alt = NULL,nb_est = NULL)"
# - Mejoradas las funciones: "func_estudio_arbolbayesiano_benefcostes()",
#     "func_estudio_entropias()", "func_estudio_arbolbayesiano_completo()"



# Funciones: Definiciones  ------------

func_distribucion_conjunta = function(probs_estados,probs_X_cond_estados) {
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  ##return( probs_estados * t(probs_X_cond_estados))
  q = dim(probs_X_cond_estados)[2];
  return(  matrix(rep(probs_estados,q),ncol=q) * probs_X_cond_estados    )
}


func_distribucion_marginal_X = function(probs_estados,probs_X_cond_estados) {
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  return( as.vector(colSums(func_distribucion_conjunta(probs_estados,probs_X_cond_estados))) )
}

func_distribucion_E_cond_X = function(probs_estados,probs_X_cond_estados) {
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  #numcol = length(marginal_X)
  num_estados = length(probs_estados)
  marginal_X_enfilas = matrix(rep(marginal_X,num_estados),nrow=num_estados,byrow=TRUE)
  conjunta = func_distribucion_conjunta(probs_estados,probs_X_cond_estados);
  return( conjunta / marginal_X_enfilas )
}

# General
# A priori
func_valoracion_esperada = function(tabla_valoraciones,probs_estados) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  return( as.vector(tabla_valoraciones %*% probs_estados) )
}


func_valoracion_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,probs_X_cond_estados) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  X_cond_estados = func_distribucion_E_cond_X(probs_estados,probs_X_cond_estados);

  return( apply(X_cond_estados,2,function(x) (func_valoracion_esperada(tabla_valoraciones,x))) )
}

func_mejor_valoracion_esperada_alternativa = function(valoracion_esperada,favorable=T) {
  if (favorable) {
    return( max(valoracion_esperada) )
  } else {
    return( min(valoracion_esperada) )
  }
}

func_mejor_alternativa = function(valoracion_esperada,favorable=T) {
  if (favorable) {
    #return( which.max(valoracion_esperada) )
    alt = which.max(valoracion_esperada)
    return(names(valoracion_esperada)[alt])
  } else {
    #return( which.min(valoracion_esperada) )
    alt = which.min(valoracion_esperada)
    return(names(valoracion_esperada)[alt])
  }
}

func_mejor_alternativa_multiple = function(valoracion_esperada,favorable=T) {
  if (favorable) {
    #return( which(valoracion_esperada==max(valoracion_esperada)) )
    alt = which(valoracion_esperada==max(valoracion_esperada))
    return(names(valoracion_esperada)[alt])

  } else {
    #return( which(valoracion_esperada==min(valoracion_esperada)) )
    alt = which(valoracion_esperada==min(valoracion_esperada))
    return(names(valoracion_esperada)[alt])
  }
}


func_valoracion_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=T) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  return( func_mejor_valoracion_esperada_alternativa(val_esperadas_apriori,favorable) )
}

func_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=T) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  #browser()
  names(val_esperadas_apriori) = rownames(tabla_valoraciones)
  return( func_mejor_alternativa(val_esperadas_apriori,favorable) )
}

func_mejor_alternativa_apriori_multiple = function(tabla_valoraciones,probs_estados,favorable=T) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  #browser()
  names(val_esperadas_apriori) = rownames(tabla_valoraciones)
  return( func_mejor_alternativa_multiple(val_esperadas_apriori,favorable) )
}


# devuelve vector con las mejores valoraciones para cada x_j de X
func_valoracion_mejor_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,
                                            probs_X_cond_estados,favorable=T) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  # favorable: TRUE beneficios y se maximiza, FALSE costos y se minimiza
  mat_val_esperadas_aposteriori = func_valoracion_esperada_a_posteriori(tabla_valoraciones,
                                            probs_estados,probs_X_cond_estados)
  return( apply(mat_val_esperadas_aposteriori,2,
                function(x) (func_mejor_valoracion_esperada_alternativa(x,favorable))) )
}

# devuelve vector con las mejores alternativas para cada x_j de X
func_mejor_alternativa_a_posteriori = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,
                                               favorable=T) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  # favorable: TRUE beneficios y se maximiza, FALSE costos y se minimiza
  #browser()
  mat_val_esperadas_aposteriori = func_valoracion_esperada_a_posteriori(tabla_valoraciones,
                                                                probs_estados,probs_X_cond_estados)
  rownames(mat_val_esperadas_aposteriori) = rownames(tabla_valoraciones)
  return( apply(mat_val_esperadas_aposteriori,2,function(x) (func_mejor_alternativa(x,favorable))) )
}


# REII: Resultado esperado con información perfecta
func_REII = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=T) {
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  val_mejor_aposteriori = func_valoracion_mejor_esperada_a_posteriori(tabla_valoraciones,probs_estados,
                                                                probs_X_cond_estados,favorable);
  return( as.numeric(marginal_X %*% val_mejor_aposteriori) )
}

# RER: Resultado esperado con riesgo  (a priori)
func_RER = function(tabla_valoraciones,probs_estados,favorable=T) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados);
  return( func_mejor_valoracion_esperada_alternativa(val_esperadas_apriori,favorable) );
}

# VEX: valor esperado máximo para el decisor por la información adicional X
func_VEX = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=T) {
  REII = func_REII(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable);
  RER = func_RER(tabla_valoraciones,probs_estados,favorable);
  return( REII - RER )
}

## función de entropía de una distribución unidimensional (marginal o condicionada)
func_entropia = function(probs,baselogaritmo=2) {
  #primera quita probabilidades nulas
  probs2 = probs[probs!=0]
  return( - sum( probs2 * log(probs2,base=baselogaritmo)  ) )
}

func_entropias_estados_cond_X = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  X_cond_estados = func_distribucion_E_cond_X(probs_estados,probs_X_cond_estados);
  return( apply(X_cond_estados,2,function(x) (func_entropia(x,baselogaritmo))) )
}


func_entropia_aposteriori = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  # tabla_valoraciones: matrix, filas alternativas, columnas estados
  # probs_estados: vector
  # probs_X_cond_estados: matrix, filas distrib. condicionadas para cada estado
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  entropias_estados_cond_X = func_entropias_estados_cond_X(probs_estados,probs_X_cond_estados,
                                                           baselogaritmo);
  return( sum(marginal_X * entropias_estados_cond_X) )
}


func_informacion_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  entropia_apriori = func_entropia(probs_estados,baselogaritmo)
  entropia_aposteriori = func_entropia_aposteriori(probs_estados,probs_X_cond_estados,baselogaritmo);
  return( entropia_apriori - entropia_aposteriori )
}

func_coeficiente_redundancia_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  entropia_apriori = func_entropia(probs_estados,baselogaritmo)
  informacion_canal = func_informacion_canal(probs_estados,probs_X_cond_estados,baselogaritmo);
  return( informacion_canal/entropia_apriori )
}


func_estudio_entropias = function(probs_estados,probs_X_cond_estados,baselogaritmo=2,
                                  nb_alt = NULL,nb_est = NULL,nb_X = NULL) {

  if (is.null(nb_est))
    nombs.E <- names(probs_estados) <- rownames(probs_X_cond_estados) <- paste("E",1:length(probs_estados),sep="")
  else
    nombs.E <- names(probs_estados) <- rownames(probs_X_cond_estados) <- nb_est

  if (is.null(nb_X))
    nombs.X <- colnames(probs_X_cond_estados) <- paste("X",1:dim(probs_X_cond_estados)[2],sep="")
  else
    nombs.X <- colnames(probs_X_cond_estados) <- nb_X

  #nombs.A <- rownames(tv) <- paste("A",1:dim(tv)[1],sep="")
  if (is.null(nb_alt))
    nombs.A <- paste("A",1:length(probs_estados),sep="")
  else
    nombs.A <- nb_alt

  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados)
  names(marginal_X) = nombs.X
  entropia_apriori = func_entropia(probs_estados,baselogaritmo)
  entropias_aposteriori = func_entropias_estados_cond_X(probs_estados,probs_X_cond_estados,baselogaritmo);
  names(entropias_aposteriori) = nombs.X
  entropia_aposteriori = func_entropia_aposteriori(probs_estados,probs_X_cond_estados,baselogaritmo);
  informacion_canal = func_informacion_canal(probs_estados,probs_X_cond_estados,baselogaritmo);
  coeficiente_redundancia = func_coeficiente_redundancia_canal(probs_estados,probs_X_cond_estados,baselogaritmo);
  resultado = list();
  resultado$marginal_X = marginal_X
  resultado$entropia_apriori =  entropia_apriori
  resultado$entropias_aposteriori =  entropias_aposteriori
  resultado$entropia_aposteriori =  entropia_aposteriori
  resultado$informacion_canal =  informacion_canal
  resultado$coeficiente_redundancia =  coeficiente_redundancia
## En porcentaje:
  resultado$coeficiente_redundancia_porc =  coeficiente_redundancia * 100
  return(resultado)
}

func_estudio_arbolbayesiano_benefcostes = function(probs_estados,probs_X_cond_estados,tv,favorable=T,
                                                   nb_alt = NULL,nb_est = NULL,nb_X = NULL) {

  if (is.null(nb_est))
    nombs.E <- names(probs_estados) <- rownames(probs_X_cond_estados) <- colnames(tv) <- paste("E",1:length(probs_estados),sep="")
  else
    nombs.E <- names(probs_estados) <- rownames(probs_X_cond_estados) <- colnames(tv) <- nb_est

  if (is.null(nb_X))
    nombs.X <- colnames(probs_X_cond_estados) <- paste("X",1:dim(probs_X_cond_estados)[2],sep="")
  else
    nombs.X <- colnames(probs_X_cond_estados) <- nb_X

  if (is.null(nb_alt))
    nombs.A <- rownames(tv) <- paste("A",1:dim(tv)[1],sep="")
  else
    nombs.A <- rownames(tv) <- nb_alt

  conjunta = func_distribucion_conjunta(probs_estados,probs_X_cond_estados)
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados)
  names(marginal_X) = nombs.X
  E_cond_X = func_distribucion_E_cond_X(probs_estados,probs_X_cond_estados)
  val_esperadas_apriori = func_valoracion_esperada(tv,probs_estados)
  names(val_esperadas_apriori) = nombs.A
  mat_val_esperadas_aposteriori = func_valoracion_esperada_a_posteriori(tv,probs_estados,probs_X_cond_estados)
  rownames(mat_val_esperadas_aposteriori) = nombs.A
  #func_mejor_valoracion_esperada_alternativa(val_esperadas_apriori,favorable)
  #func_mejor_alternativa(val_esperadas_apriori,favorable)
  #func_mejor_alternativa_multiple(val_esperadas_apriori,favorable)

  mejor_val_alt_apriori = func_valoracion_mejor_alternativa_apriori(tv,probs_estados,favorable)
  #func_mejor_alternativa_apriori(tv,probs_estados,favorable)
  mejor_alts_apriori = func_mejor_alternativa_apriori_multiple(tv,probs_estados,favorable)

  mejor_val_alt_aposteriori = func_valoracion_mejor_esperada_a_posteriori(tv,probs_estados,probs_X_cond_estados,favorable)
  mejor_alt_aposteriori = func_mejor_alternativa_a_posteriori(tv,probs_estados,probs_X_cond_estados,favorable)

  valor_REII = func_REII(tv,probs_estados,probs_X_cond_estados,favorable)
  valor_RER = func_RER(tv,probs_estados,favorable)
  valor_VEX = func_VEX(tv,probs_estados,probs_X_cond_estados,favorable)
  valor_VEX_Xi = data.frame(
                            REMAXi = rep(0,length(nombs.X)),
                            REaltpriori = rep(0,length(nombs.X)),
                            VEXi = rep(0,length(nombs.X))
                            )
  rownames(valor_VEX_Xi) = nombs.X
  for (i in 1:length(nombs.X)) {
    vxi = func_VEIP_tv(tv,
                       probs_estados = E_cond_X[,nombs.X[i]],
                       favorable = favorable,
                       que_alt_Ref = mejor_alts_apriori[1],
                       nb_alt = nb_alt,nb_est = nb_est)
    valor_VEX_Xi$REaltpriori[i] = vxi[[1]]
    valor_VEX_Xi$REMAXi[i] = vxi[[2]]
    valor_VEX_Xi$VEXi[i] = vxi[[3]]
  }
  if (length(mejor_alts_apriori)>1) { # solución a priori múltiple
      warning("Soluciones múltiples en estudio a priori! ",
              paste(mejor_alts_apriori,", "),
              " no sería correcto el cálculo de valor_VEX_Xi" )
  }
  valor_VEX_Xi = as.matrix.data.frame(valor_VEX_Xi)


resultados = list()
resultados$probs_conjunta = conjunta
resultados$probs_marginal_X = marginal_X
resultados$probs_E_cond_X = E_cond_X
resultados$val_esperadas_apriori = val_esperadas_apriori
  resultados$mejor_val_alt_apriori = mejor_val_alt_apriori
  resultados$mejor_alts_apriori = mejor_alts_apriori
resultados$mat_val_esperadas_aposteriori = mat_val_esperadas_aposteriori
  resultados$mejor_val_alt_aposteriori = mejor_val_alt_aposteriori
  resultados$mejor_alt_aposteriori = mejor_alt_aposteriori
  resultados$valor_REII = valor_REII
  resultados$valor_RER = valor_RER
  resultados$valor_VEX = valor_VEX
  resultados$valor_VEX_Xi = valor_VEX_Xi
return(resultados)

}

func_estudio_arbolbayesiano_completo = function(probs_estados,probs_X_cond_estados,tv,favorable=T,baselogaritmo=2,
                                                nb_alt = NULL,nb_est = NULL,nb_X = NULL) {

  resultados = list()
  #browser()
  resultados$informe_benefcostes = func_estudio_arbolbayesiano_benefcostes(probs_estados,probs_X_cond_estados,tv,favorable,nb_alt,nb_est,nb_X)
  resultados$informe_entropia = func_estudio_entropias(probs_estados,probs_X_cond_estados,baselogaritmo,nb_alt,nb_est,nb_X)

  return(resultados)
}


func_VEIP_tv = function(tv,probs_estados,favorable=TRUE,que_alt_Ref = NULL,
                        nb_alt = NULL,nb_est = NULL) {

  if (is.null(nb_est))
    nombs.E <- names(probs_estados) <- colnames(tv) <- paste("E",1:length(probs_estados),sep="")
  else
    nombs.E <- names(probs_estados) <- colnames(tv) <- nb_est

  if (is.null(nb_alt))
    nombs.A <- rownames(tv) <- paste("A",1:dim(tv)[1],sep="")
  else
    nombs.A <- rownames(tv) <- nb_alt

  REMA = func_valoracion_mejor_alternativa_apriori(tv,probs_estados,favorable)

  if (is.null(que_alt_Ref)) {
    if (favorable) {
      mejores = apply(tv,MARGIN = 2,max)
      REIP = sum(mejores * probs_estados)
    } else {
      mejores = apply(tv,MARGIN = 2,min)
      REIP = sum(mejores * probs_estados)
    }
  } else {
    REIP = REMA
    REMA = sum( tv[que_alt_Ref,] * probs_estados)
  }
  VEIP = REIP - REMA

  result = list()
  result$REMA = REMA
  result$REIP = REIP
  result$VEIP = VEIP

  return(result)
}


