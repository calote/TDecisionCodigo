#
# PROBLEMAS DE ARBOLES DE DECISION BAYESIANOS EN AMBIENTE DE RIESGO ------
#

## Funciones para el analisis de arboles de decision bayesianos
## Incluye calculos de probabilidades conjuntas, marginales y condicionadas,
## valoraciones esperadas a priori y a posteriori, entropia e informacion del canal.


# Funciones: Definiciones  ------------

#' Distribucion de probabilidad conjunta
#'
#' Calcula la distribucion de probabilidad conjunta de los estados naturales
#' y las modalidades de la variable observadora X.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori de cada
#'   estado natural. Debe sumar 1.
#' @param probs_X_cond_estados Matriz numerica donde cada fila representa la
#'   distribucion condicional de las modalidades de X dado un estado natural.
#'   Las filas deben sumar 1.
#'
#' @return Matriz numerica de dimension (n_estados x n_modalidades_X) con las
#'   probabilidades conjuntas P(E_i, X_j).
#'
#' @examples
#' # Ejemplo: 3 estados, 2 modalidades de X
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2,
#'                        0.4, 0.6,
#'                        0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_distribucion_conjunta(probs_E, probs_XdE)
#'
#' @export
func_distribucion_conjunta = function(probs_estados,probs_X_cond_estados) {
  q = dim(probs_X_cond_estados)[2];
  return(  matrix(rep(probs_estados,q),ncol=q) * probs_X_cond_estados    )
}


#' Distribucion marginal de la variable X
#'
#' Calcula la distribucion marginal de las modalidades de la variable X
#' a partir de las probabilidades de los estados y las distribuciones condicionales.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori de cada
#'   estado natural.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales
#'   de X dado cada estado.
#'
#' @return Vector numerico con las probabilidades marginales de cada modalidad
#'   de X (P(X_j)).
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_distribucion_marginal_X(probs_E, probs_XdE)
#'
#' @export
func_distribucion_marginal_X = function(probs_estados,probs_X_cond_estados) {
  return( as.vector(colSums(func_distribucion_conjunta(probs_estados,probs_X_cond_estados))) )
}


#' Distribucion condicionada de los estados dado X
#'
#' Calcula las probabilidades a posteriori de cada estado natural condicionadas
#' a cada modalidad de la variable observadora X.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori de cada
#'   estado natural.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales
#'   de X dado cada estado.
#'
#' @return Matriz numerica de dimension (n_estados x n_modalidades_X) con las
#'   probabilidades condicionadas P(E_i | X_j).
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_distribucion_E_cond_X(probs_E, probs_XdE)
#'
#' @export
func_distribucion_E_cond_X = function(probs_estados,probs_X_cond_estados) {
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  num_estados = length(probs_estados)
  marginal_X_enfilas = matrix(rep(marginal_X,num_estados),nrow=num_estados,byrow=TRUE)
  conjunta = func_distribucion_conjunta(probs_estados,probs_X_cond_estados);
  return( conjunta / marginal_X_enfilas )
}


#' Valoracion esperada a priori
#'
#' Calcula la valoracion esperada de cada alternativa de decision a priori,
#' ponderando las valoraciones por las probabilidades de los estados naturales.
#'
#' @param tabla_valoraciones Matriz numerica donde las filas representan las
#'   alternativas y las columnas los estados naturales. Cada elemento es la
#'   valoracion de la alternativa i ante el estado j.
#' @param probs_estados Vector numerico con las probabilidades a priori de cada
#'   estado natural.
#'
#' @return Vector numerico con las valoraciones esperadas de cada alternativa.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' func_valoracion_esperada(TV, probs_E)
#'
#' @export
func_valoracion_esperada = function(tabla_valoraciones,probs_estados) {
  return( as.vector(tabla_valoraciones %*% probs_estados) )
}


#' Valoracion esperada a posteriori
#'
#' Calcula las valoraciones esperadas de cada alternativa para cada modalidad
#' de la variable X, usando las probabilidades condicionadas a posteriori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de las
#'   alternativas ante cada estado.
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#'
#' @return Matriz numerica donde cada columna contiene las valoraciones esperadas
#'   a posteriori para una modalidad de X.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_valoracion_esperada_a_posteriori(TV, probs_E, probs_XdE)
#'
#' @export
func_valoracion_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,probs_X_cond_estados) {
  X_cond_estados = func_distribucion_E_cond_X(probs_estados,probs_X_cond_estados);
  return( apply(X_cond_estados,2,function(x) (func_valoracion_esperada(tabla_valoraciones,x))) )
}


#' Mejor valoracion esperada entre alternativas
#'
#' Devuelve la mejor valoracion esperada entre todas las alternativas.
#'
#' @param valoracion_esperada Vector numerico con las valoraciones esperadas.
#' @param favorable Logico. Si TRUE (por defecto), se maximiza (problema de
#'   beneficios). Si FALSE, se minimiza (problema de costes).
#'
#' @return Valor numerico con la mejor valoracion esperada.
#'
#' @examples
#' ve <- c(A1 = 65, A2 = 70, A3 = 55)
#' func_mejor_valoracion_esperada_alternativa(ve, favorable = TRUE)
#'
#' @export
func_mejor_valoracion_esperada_alternativa = function(valoracion_esperada,favorable=TRUE) {
  if (favorable) {
    return( max(valoracion_esperada) )
  } else {
    return( min(valoracion_esperada) )
  }
}


#' Nombre de la mejor alternativa
#'
#' Devuelve el nombre de la alternativa con la mejor valoracion esperada.
#'
#' @param valoracion_esperada Vector numerico con las valoraciones esperadas,
#'   con nombres que identifican las alternativas.
#' @param favorable Logico. Si TRUE, se busca la maxima. Si FALSE, la minima.
#'
#' @return Character string con el nombre de la mejor alternativa.
#'
#' @examples
#' ve <- c(A1 = 65, A2 = 70, A3 = 55)
#' func_mejor_alternativa(ve, favorable = TRUE)
#'
#' @export
func_mejor_alternativa = function(valoracion_esperada,favorable=TRUE) {
  if (favorable) {
    alt = which.max(valoracion_esperada)
    return(names(valoracion_esperada)[alt])
  } else {
    alt = which.min(valoracion_esperada)
    return(names(valoracion_esperada)[alt])
  }
}


#' Nombres de las mejores alternativas (empate)
#'
#' Devuelve los nombres de todas las alternativas que empatan en la mejor
#' valoracion esperada.
#'
#' @param valoracion_esperada Vector numerico con las valoraciones esperadas.
#' @param favorable Logico. Si TRUE, busca las maximas. Si FALSE, las minimas.
#'
#' @return Vector de character strings con los nombres de las alternativas
#'   optimas (puede contener mas de uno si hay empate).
#'
#' @examples
#' ve <- c(A1 = 70, A2 = 70, A3 = 55)
#' func_mejor_alternativa_multiple(ve, favorable = TRUE)
#'
#' @export
func_mejor_alternativa_multiple = function(valoracion_esperada,favorable=TRUE) {
  if (favorable) {
    alt = which(valoracion_esperada==max(valoracion_esperada))
    return(names(valoracion_esperada)[alt])
  } else {
    alt = which(valoracion_esperada==min(valoracion_esperada))
    return(names(valoracion_esperada)[alt])
  }
}


#' Mejor valoracion esperada a priori
#'
#' Calcula la mejor valoracion esperada entre todas las alternativas a priori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas
#'   ante cada estado.
#' @param probs_estados Vector numerico con las probabilidades de los estados.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Valor numerico con la mejor valoracion esperada a priori.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' probs_E <- c(0.3, 0.5, 0.2)
#' func_valoracion_mejor_alternativa_apriori(TV, probs_E, favorable = TRUE)
#'
#' @export
func_valoracion_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=TRUE) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  return( func_mejor_valoracion_esperada_alternativa(val_esperadas_apriori,favorable) )
}


#' Mejor alternativa a priori
#'
#' Devuelve el nombre de la alternativa con la mejor valoracion esperada a priori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas
#'   ante cada estado.
#' @param probs_estados Vector numerico con las probabilidades de los estados.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Character string con el nombre de la mejor alternativa a priori.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' func_mejor_alternativa_apriori(TV, probs_E, favorable = TRUE)
#'
#' @export
func_mejor_alternativa_apriori = function(tabla_valoraciones,probs_estados,favorable=TRUE) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  names(val_esperadas_apriori) = rownames(tabla_valoraciones)
  return( func_mejor_alternativa(val_esperadas_apriori,favorable) )
}


#' Mejores alternativas a priori (multiples)
#'
#' Devuelve los nombres de todas las alternativas que empatan en la mejor
#' valoracion esperada a priori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades de los estados.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Vector de character strings con los nombres de las alternativas optimas.
#'
#' @examples
#' TV <- matrix(c(100, 50, 100, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' func_mejor_alternativa_apriori_multiple(TV, probs_E, favorable = TRUE)
#'
#' @export
func_mejor_alternativa_apriori_multiple = function(tabla_valoraciones,probs_estados,favorable=TRUE) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados)
  names(val_esperadas_apriori) = rownames(tabla_valoraciones)
  return( func_mejor_alternativa_multiple(val_esperadas_apriori,favorable) )
}


#' Mejores valoraciones esperadas a posteriori
#'
#' Para cada modalidad de la variable X, devuelve la mejor valoracion esperada
#' entre todas las alternativas usando las probabilidades a posteriori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Vector numerico con la mejor valoracion esperada a posteriori para
#'   cada modalidad de X.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_valoracion_mejor_esperada_a_posteriori(TV, probs_E, probs_XdE, favorable = TRUE)
#'
#' @export
func_valoracion_mejor_esperada_a_posteriori = function(tabla_valoraciones,probs_estados,
                                            probs_X_cond_estados,favorable=TRUE) {
  mat_val_esperadas_aposteriori = func_valoracion_esperada_a_posteriori(tabla_valoraciones,
                                            probs_estados,probs_X_cond_estados)
  return( apply(mat_val_esperadas_aposteriori,2,
                function(x) (func_mejor_valoracion_esperada_alternativa(x,favorable))) )
}


#' Mejores alternativas a posteriori
#'
#' Para cada modalidad de la variable X, devuelve el nombre de la alternativa
#' con la mejor valoracion esperada a posteriori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Vector de character strings con el nombre de la mejor alternativa
#'   para cada modalidad de X.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_mejor_alternativa_a_posteriori(TV, probs_E, probs_XdE, favorable = TRUE)
#'
#' @export
func_mejor_alternativa_a_posteriori = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,
                                               favorable=TRUE) {
  mat_val_esperadas_aposteriori = func_valoracion_esperada_a_posteriori(tabla_valoraciones,
                                                                probs_estados,probs_X_cond_estados)
  rownames(mat_val_esperadas_aposteriori) = rownames(tabla_valoraciones)
  return( apply(mat_val_esperadas_aposteriori,2,function(x) (func_mejor_alternativa(x,favorable))) )
}


#' Resultado Esperado con Informacion Perfecta (REII)
#'
#' Calcula el resultado esperado cuando se dispone de informacion perfecta
#' sobre el estado de la naturaleza antes de decidir.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Valor numerico con el REII.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_REII(TV, probs_E, probs_XdE, favorable = TRUE)
#'
#' @export
func_REII = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=TRUE) {
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  val_mejor_aposteriori = func_valoracion_mejor_esperada_a_posteriori(tabla_valoraciones,probs_estados,
                                                                probs_X_cond_estados,favorable);
  return( as.numeric(marginal_X %*% val_mejor_aposteriori) )
}


#' Resultado Esperado con Riesgo (RER)
#'
#' Calcula el resultado esperado bajo incertidumbre (sin informacion adicional).
#' Es la mejor valoracion esperada a priori.
#'
#' @param tabla_valoraciones Matriz numerica con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades de los estados.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Valor numerico con el RER.
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' probs_E <- c(0.3, 0.5, 0.2)
#' func_RER(TV, probs_E, favorable = TRUE)
#'
#' @export
func_RER = function(tabla_valoraciones,probs_estados,favorable=TRUE) {
  val_esperadas_apriori = func_valoracion_esperada(tabla_valoraciones,probs_estados);
  return( func_mejor_valoracion_esperada_alternativa(val_esperadas_apriori,favorable) );
}


#' Valor Esperado de la Informacion Adicional (VEX)
#'
#' Calcula el valor esperado maximo que el decisor estaria dispuesto a pagar
#' por obtener informacion adicional X antes de decidir.
#'
#' @param tabla_valoraciones Matriz numerico con las valoraciones de alternativas.
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#'
#' @return Valor numerico con el VEX (REII - RER).
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_VEX(TV, probs_E, probs_XdE, favorable = TRUE)
#'
#' @export
func_VEX = function(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable=TRUE) {
  REII = func_REII(tabla_valoraciones,probs_estados,probs_X_cond_estados,favorable);
  RER = func_RER(tabla_valoraciones,probs_estados,favorable);
  return( REII - RER )
}


#' Entropia de una distribucion
#'
#' Calcula la entropia de Shannon de una distribucion de probabilidad unidimensional.
#'
#' @param probs Vector numerico con las probabilidades de la distribucion.
#'   Los valores nulos se ignoran automaticamente.
#' @param baselogaritmo Numerico. Base del logaritmo para el calculo de la
#'   entropia. Por defecto 2 (bits).
#'
#' @return Valor numerico con la entropia de la distribucion.
#'
#' @examples
#' # Entropia maxima: distribucion uniforme
#' func_entropia(c(0.5, 0.5))
#' # Entropia minima: distribucion degenerada
#' func_entropia(c(1, 0))
#' # Entropia en nats (base e)
#' func_entropia(c(0.5, 0.5), baselogaritmo = exp(1))
#'
#' @export
func_entropia = function(probs,baselogaritmo=2) {
  probs2 = probs[probs!=0]
  return( - sum( probs2 * log(probs2,base=baselogaritmo)  ) )
}


#' Entropias condicionadas de los estados dado X
#'
#' Calcula la entropia de la distribucion de estados para cada modalidad
#' de la variable X.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param baselogaritmo Numerico. Base del logaritmo. Por defecto 2.
#'
#' @return Vector numerico con las entropias condicionadas para cada modalidad
#'   de X.
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_entropias_estados_cond_X(probs_E, probs_XdE)
#'
#' @export
func_entropias_estados_cond_X = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  X_cond_estados = func_distribucion_E_cond_X(probs_estados,probs_X_cond_estados);
  return( apply(X_cond_estados,2,function(x) (func_entropia(x,baselogaritmo))) )
}


#' Entropia a posteriori
#'
#' Calcula la entropia condicional promedio de la distribucion de estados
#' dado X, promediada sobre las modalidades de X.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param baselogaritmo Numerico. Base del logaritmo. Por defecto 2.
#'
#' @return Valor numerico con la entropia a posteriori.
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_entropia_aposteriori(probs_E, probs_XdE)
#'
#' @export
func_entropia_aposteriori = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  marginal_X = func_distribucion_marginal_X(probs_estados,probs_X_cond_estados);
  entropias_estados_cond_X = func_entropias_estados_cond_X(probs_estados,probs_X_cond_estados,
                                                           baselogaritmo);
  return( sum(marginal_X * entropias_estados_cond_X) )
}


#' Informacion del canal
#'
#' Calcula la informacion mutua (reduccion de entropia) que proporciona la
#' variable X sobre la distribucion de los estados naturales.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param baselogaritmo Numerico. Base del logaritmo. Por defecto 2.
#'
#' @return Valor numerico con la informacion del canal (H(E) - H(E|X)).
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_informacion_canal(probs_E, probs_XdE)
#'
#' @export
func_informacion_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  entropia_apriori = func_entropia(probs_estados,baselogaritmo)
  entropia_aposteriori = func_entropia_aposteriori(probs_estados,probs_X_cond_estados,baselogaritmo);
  return( entropia_apriori - entropia_aposteriori )
}


#' Coeficiente de redundancia del canal
#'
#' Calcula el coeficiente de redundancia normalizado, que mide la fraccion
#' de entropia a priori que es eliminada por la informacion de X.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param baselogaritmo Numerico. Base del logaritmo. Por defecto 2.
#'
#' @return Valor numerico entre 0 y 1 con el coeficiente de redundancia.
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' func_coeficiente_redundancia_canal(probs_E, probs_XdE)
#'
#' @export
func_coeficiente_redundancia_canal = function(probs_estados,probs_X_cond_estados,baselogaritmo=2) {
  entropia_apriori = func_entropia(probs_estados,baselogaritmo)
  informacion_canal = func_informacion_canal(probs_estados,probs_X_cond_estados,baselogaritmo);
  return( informacion_canal/entropia_apriori )
}


#' Estudio completo de entropias
#'
#' Realiza un analisis completo de entropias incluyendo entropia a priori,
#' entropias a posteriori, entropia a posteriori promedio, informacion del canal
#' y coeficiente de redundancia.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param baselogaritmo Numerico. Base del logaritmo. Por defecto 2.
#' @param nb_alt Vector de character strings opcional con los nombres de las
#'   alternativas.
#' @param nb_est Vector de character strings opcional con los nombres de los
#'   estados.
#' @param nb_X Vector de character strings opcional con los nombres de las
#'   modalidades de X.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{marginal_X}{Distribucion marginal de X}
#'   \item{entropia_apriori}{Entropia a priori de los estados}
#'   \item{entropias_aposteriori}{Entropias condicionadas para cada X_j}
#'   \item{entropia_aposteriori}{Entropia a posteriori promedio}
#'   \item{informacion_canal}{Informacion mutua I(E;X)}
#'   \item{coeficiente_redundancia}{Coeficiente normalizado}
#'   \item{coeficiente_redundancia_porc}{Coeficiente en porcentaje}
#' }
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' resultado <- func_estudio_entropias(probs_E, probs_XdE)
#' resultado$entropia_apriori
#' resultado$informacion_canal
#'
#' @export
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
  resultado$coeficiente_redundancia_porc =  coeficiente_redundancia * 100
  return(resultado)
}


#' Estudio de arbol bayesiano con beneficios/costes
#'
#' Realiza un analisis completo de un problema de decision bayesiano
#' incluyendo distribuciones probabilisticas, valoraciones esperadas
#' a priori y a posteriori, y medidas de valor de la informacion.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori de los
#'   estados naturales.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales
#'   de las modalidades de X dado cada estado.
#' @param tv Matriz numerica de valoraciones donde las filas son alternativas
#'   y las columnas estados.
#' @param favorable Logico. Si TRUE, se trata un problema de beneficios
#'   (se maximiza). Si FALSE, de costes (se minimiza).
#' @param nb_alt Vector de character strings opcional con los nombres de las
#'   alternativas.
#' @param nb_est Vector de character strings opcional con los nombres de los
#'   estados.
#' @param nb_X Vector de character strings opcional con los nombres de las
#'   modalidades de X.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{probs_conjunta}{Distribucion conjunta P(E_i, X_j)}
#'   \item{probs_marginal_X}{Distribucion marginal P(X_j)}
#'   \item{probs_E_cond_X}{Distribuciones condicionadas P(E_i | X_j)}
#'   \item{val_esperadas_apriori}{Valoraciones esperadas a priori}
#'   \item{mejor_val_alt_apriori}{Mejor valoracion a priori}
#'   \item{mejor_alts_apriori}{Mejores alternativas a priori}
#'   \item{mat_val_esperadas_aposteriori}{Matriz de valoraciones a posteriori}
#'   \item{mejor_val_alt_aposteriori}{Vector de mejores valoraciones a posteriori}
#'   \item{mejor_alt_aposteriori}{Vector de mejores alternativas a posteriori}
#'   \item{valor_REII}{Resultado Esperado con Informacion Perfecta}
#'   \item{valor_RER}{Resultado Esperado con Riesgo}
#'   \item{valor_VEX}{Valor Esperado de la Informacion Adicional}
#'   \item{valor_VEX_Xi}{Desglose del VEX por cada modalidad de X}
#' }
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' resultado <- func_estudio_arbolbayesiano_benefcostes(probs_E, probs_XdE, TV, favorable = TRUE)
#' resultado$valor_VEX
#'
#' @export
func_estudio_arbolbayesiano_benefcostes = function(probs_estados,probs_X_cond_estados,tv,favorable=TRUE,
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

  mejor_val_alt_apriori = func_valoracion_mejor_alternativa_apriori(tv,probs_estados,favorable)
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
  if (length(mejor_alts_apriori)>1) {
      warning("Soluciones multiples en estudio a priori! ",
              paste(mejor_alts_apriori,", "),
              " no seria correcto el calculo de valor_VEX_Xi" )
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


#' Estudio completo de arbol bayesiano
#'
#' Realiza un analisis completo de un arbol de decision bayesiano que incluye
#' tanto el estudio de beneficios/costes como el analisis de entropias.
#'
#' @param probs_estados Vector numerico con las probabilidades a priori.
#' @param probs_X_cond_estados Matriz numerica con las distribuciones condicionales.
#' @param tv Matriz numerica de valoraciones.
#' @param favorable Logico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#' @param baselogaritmo Numerico. Base del logaritmo para entropias. Por defecto 2.
#' @param nb_alt Vector de character strings opcional con nombres de alternativas.
#' @param nb_est Vector de character strings opcional con nombres de estados.
#' @param nb_X Vector de character strings opcional con nombres de modalidades de X.
#'
#' @return Lista con dos sublistas:
#' \describe{
#'   \item{informe_benefcostes}{Resultado de \code{func_estudio_arbolbayesiano_benefcostes}}
#'   \item{informe_entropia}{Resultado de \code{func_estudio_entropias}}
#' }
#'
#' @examples
#' probs_E <- c(0.3, 0.5, 0.2)
#' probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9), nrow = 3, byrow = TRUE)
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' resultado <- func_estudio_arbolbayesiano_completo(probs_E, probs_XdE, TV, favorable = TRUE)
#' names(resultado)
#'
#' @export
func_estudio_arbolbayesiano_completo = function(probs_estados,probs_X_cond_estados,tv,favorable=TRUE,baselogaritmo=2,
                                                nb_alt = NULL,nb_est = NULL,nb_X = NULL) {

  resultados = list()
  resultados$informe_benefcostes = func_estudio_arbolbayesiano_benefcostes(probs_estados,probs_X_cond_estados,tv,favorable,nb_alt,nb_est,nb_X)
  resultados$informe_entropia = func_estudio_entropias(probs_estados,probs_X_cond_estados,baselogaritmo,nb_alt,nb_est,nb_X)

  return(resultados)
}


#' Valor Esperado de Informacion Perfecta con tabla de valoraciones
#'
#' Calcula el REIP (Resultado Esperado con Informacion Perfecta) y el VEIP
#' (Valor Esperado de Informacion Perfecta) a partir de una tabla de
#' valoraciones y probabilidades.
#'
#' @param tv Matriz numerica de valoraciones donde las filas son alternativas
#'   y las columnas estados.
#' @param probs_estados Vector numerico con las probabilidades de los estados.
#' @param favorable Logico. Si TRUE, se maximiza. Si FALSE, se minimiza.
#' @param que_alt_Ref Character string opcional con el nombre de la alternativa
#'   de referencia. Si es NULL, se usa la mejor alternativa a priori.
#' @param nb_alt Vector de character strings opcional con nombres de alternativas.
#' @param nb_est Vector de character strings opcional con nombres de estados.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{REMA}{Resultado Esperado de la Mejor Alternativa a priori}
#'   \item{REIP}{Resultado Esperado con Informacion Perfecta}
#'   \item{VEIP}{Valor Esperado de Informacion Perfecta (REIP - REMA)}
#' }
#'
#' @examples
#' TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
#' rownames(TV) <- c("A1", "A2")
#' probs_E <- c(0.3, 0.5, 0.2)
#' resultado <- func_VEIP_tv(TV, probs_E, favorable = TRUE)
#' resultado$VEIP
#'
#' @export
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
