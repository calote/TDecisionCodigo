
## FUNCIONES METODOS MULTICRITERIOS

## Nota: mayor utilidad (mayor valor) mejor

## Validaciones internas ------------------------------------------------------

validar_matriz_numerica <- function(X, cuadrada = FALSE, positiva = FALSE) {
  es_tabla_numerica <- is.matrix(X) || is.data.frame(X)
  valores <- if (is.data.frame(X)) unlist(X, use.names = FALSE) else X
  if (!es_tabla_numerica || !is.numeric(valores) || any(!is.finite(valores))) {
    stop("X debe ser una matriz numerica finita.", call. = FALSE)
  }
  if (cuadrada && nrow(X) != ncol(X)) {
    stop("X debe ser una matriz cuadrada.", call. = FALSE)
  }
  if (positiva && any(X <= 0)) {
    stop("X debe contener valores estrictamente positivos.", call. = FALSE)
  }
  invisible(X)
}

validar_pesos <- function(pesos, n) {
  if (!is.numeric(pesos) || length(pesos) != n ||
      any(!is.finite(pesos)) || any(pesos < 0) || sum(pesos) <= 0) {
    stop("Los pesos deben ser numericos, no negativos y compatibles con los criterios.",
         call. = FALSE)
  }
  invisible(pesos)
}


#### Funciones de creacion de matrices ----

#' Crear matriz de valoraciones (diagonal completa)
#'
#' Crea una matriz cuadrada de valoraciones donde cada elemento indica la
#' relacion de preferencia entre alternativas.
#'
#' @param vector_matporfilas Vector numerico con los valores de la matriz
#'   rellenable por filas.
#' @param numalternativas Numerico. Numero de alternativas (filas y columnas).
#' @param v.nombres.alternativas Vector de character strings opcional con los
#'   nombres de las alternativas.
#'
#' @return Matriz cuadrada numerica con las valoraciones y nombres asignados.
#'
#' @examples
#' multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("Rendimiento","Riesgo"))
#' multicriterio.crea.matrizvaloraciones(c(1,1,1,0,1,0,1,0,1), 3)
#'
#' @export
multicriterio.crea.matrizvaloraciones = function(vector_matporfilas,numalternativas=4,
                                                  v.nombres.alternativas=NULL) {
  if (length(numalternativas) != 1 || numalternativas < 1 ||
      numalternativas != as.integer(numalternativas)) {
    stop("numalternativas debe ser un entero positivo.", call. = FALSE)
  }
  if (length(vector_matporfilas) != numalternativas^2) {
    stop("El vector debe contener numalternativas^2 valores.", call. = FALSE)
  }
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numalternativas,byrow=TRUE);
  if (is.null(v.nombres.alternativas)) {
    colnames(X) = paste('a',1:numalternativas,sep='');
    rownames(X) = colnames(X)
  } else{
    colnames(X) = v.nombres.alternativas
    rownames(X) = colnames(X)
  }
  return(X)

}


#' Crear matriz de valoraciones (solo diagonal superior)
#'
#' Crea una matriz cuadrada de valoraciones a partir unicamente de los
#' valores de la diagonal superior. Los valores inferiores se calculan
#' como el inverso.
#'
#' @param vector_valoraciones_diagsup Vector numerico con los valores de la
#'   diagonal superior (sin incluir la diagonal de unos).
#' @param numalternativas Numerico. Numero de alternativas.
#' @param v.nombres.alternativas Vector de character strings opcional con los
#'   nombres de las alternativas.
#'
#' @return Matriz cuadrada numerica completa con las valoraciones.
#'
#' @examples
#' v_val <- c(5, 3, 7, 3, 3, 6, 9, 1, 3, 1)
#' multicriterio.crea.matrizvaloraciones_mej(v_val, numalternativas = 5)
#'
#' @export
multicriterio.crea.matrizvaloraciones_mej = function(vector_valoraciones_diagsup,numalternativas=4,v.nombres.alternativas = NULL) {
  if (length(numalternativas) != 1 || numalternativas < 1 ||
      numalternativas != as.integer(numalternativas)) {
    stop("numalternativas debe ser un entero positivo.", call. = FALSE)
  }
  if (length(vector_valoraciones_diagsup) != numalternativas * (numalternativas - 1) / 2) {
    stop("El vector debe contener los valores de la diagonal superior.", call. = FALSE)
  }

  if (is.null(v.nombres.alternativas)) {
    nbalt = paste0("a",1:numalternativas)
  } else {
    nbalt = v.nombres.alternativas
  }
  mat <- matrix(1, nrow = numalternativas, ncol = numalternativas, byrow = TRUE, dimnames = list(nbalt, nbalt))
  nn = 1
  for (i in 1:(numalternativas-1)) {
    for (j in (i+1):(numalternativas)) {
      mat[i,j] = vector_valoraciones_diagsup[nn]
      mat[j,i] = 1/vector_valoraciones_diagsup[nn]
      nn = nn+1
    }
  }
  return(mat)
}


#' Crear matriz de decision
#'
#' Crea una matriz de decision donde las filas son alternativas y las
#' columnas son criterios.
#'
#' @param vector_matporfilas Vector numerico con los valores de la matriz
#'   rellenable por filas.
#' @param numalternativas Numerico. Numero de alternativas (filas).
#' @param numcriterios Numerico. Numero de criterios (columnas).
#' @param v.nombresalt Vector de character strings opcional con nombres de alternativas.
#' @param v.nombrescri Vector de character strings opcional con nombres de criterios.
#'
#' @return Matriz numerica con los datos de decision.
#'
#' @examples
#' multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#'
#' @export
multicriterio.crea.matrizdecision = function(vector_matporfilas,numalternativas=3,
                                     numcriterios=4,v.nombresalt=NULL,v.nombrescri=NULL) {
  if (length(numalternativas) != 1 || length(numcriterios) != 1 ||
      numalternativas < 1 || numcriterios < 1 ||
      numalternativas != as.integer(numalternativas) ||
      numcriterios != as.integer(numcriterios)) {
    stop("Las dimensiones deben ser enteros positivos.", call. = FALSE)
  }
  if (length(vector_matporfilas) != numalternativas * numcriterios) {
    stop("El vector no es compatible con las dimensiones indicadas.", call. = FALSE)
  }

  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numcriterios,byrow=TRUE)
  if (is.null(v.nombrescri)) {
    colnames(X) = paste('C',1:numcriterios,sep='');
  } else{
    colnames(X) = v.nombrescri
  }
  if (is.null(v.nombresalt)) {
    rownames(X) = paste('a',1:numalternativas,sep='');
  } else{
    rownames(X) = v.nombresalt
  }
  return(X);

}


## Metodos de construccion de funciones de valor o utilidad ------

#' Construccion de funcion de utilidad maximales
#'
#' Construye una funcion de utilidad a partir de una matriz de valoraciones
#' usando el metodo de maximales (no borrosos).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de valoraciones entre alternativas.
#'
#' @return Vector numerico con los valores de utilidad de cada alternativa,
#'   ordenados de mayor a menor.
#'
#' @examples
#' vector <- c(1,1,1,0,1,0,1,0,0,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 5)
#' multicriterio.constfuncutilidad.maximales(Xmat)
#'
#' @export
multicriterio.constfuncutilidad.maximales = function(Xmatrizvaloraciones) {
  A1 = Xmatrizvaloraciones
  futil = rep(NA,ncol(A1))
  names(futil) = colnames(A1)
  A1p1 = A1
  diag(A1p1) = rep(0,ncol(A1))
  while (!is.null(dim(A1p1))) {
    alts_min = which(colSums(A1p1)==min(colSums(A1p1)))
    if (length(alts_min)>1) {
      A2 = A1p1[alts_min,alts_min]
      Dom = rep(FALSE,length(alts_min))
      for (i in 1:length(alts_min)) {
        cand = which(A2[,i]==1)
        for (j in cand) {
          if (A2[i,j]==1) {
          } else {
            Dom[i] = TRUE
          }
        }
      }
      alts_min = alts_min[!Dom]
    }
    for (k in 1:length(alts_min)) futil[names(alts_min)[k]] = ncol(A1p1)
    if (length(alts_min)<ncol(A1p1)) {
      A1p1 = A1p1[-alts_min,-alts_min]
    } else {
      A1p1 = NULL
    }
  }
  futil[is.na(futil)] = 1
  return(futil)
}


#' Construccion de funcion de utilidad con estructura borrosa
#'
#' Construye una funcion de utilidad a partir de una matriz de valoraciones
#' usando el metodo de flujo neto (estructura borrosa).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de valoraciones entre alternativas.
#'
#' @return Vector numerico con los flujos netos de cada alternativa (valores
#'   de utilidad).
#'
#' @examples
#' vector <- c(0,0.6,0.2,0.8,0.4,0,0.2,0,0.5,0.1,0,0.1,0.2,0,0.1,0)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 4)
#' multicriterio.constfuncutilidad.estructuraborrosa(Xmat)
#'
#' @export
multicriterio.constfuncutilidad.estructuraborrosa = function(Xmatrizvaloraciones) {
  X = Xmatrizvaloraciones
  flujo.saliente = rowSums(X);
  flujo.entrante = colSums(X);
  flujo.neto = flujo.saliente-flujo.entrante;
  return(flujo.neto)
}


## Homogeneizacion tabla decision ------

#' Homogeneizacion por metodo Nadir
#'
#' Normaliza una matriz de decision usando el metodo de escalado min-max
#' (Nadir), transformando todos los valores al rango `[0, 1]`.
#'
#' @param Xmatrizdecision Matriz numerica de decision (alternativas x criterios).
#'
#' @return Matriz numerica homogeneizada con valores en `[0, 1]`.
#'
#' @examples
#' Xmat <- multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#' multicriterio.homogeneizacion.nadir(Xmat)
#'
#' @export
multicriterio.homogeneizacion.nadir = function(Xmatrizdecision) {
  validar_matriz_numerica(Xmatrizdecision)
  X = Xmatrizdecision
  (vcol.max = apply(X,2,max))
  (vcol.min = apply(X,2,min))
  numfilas = nrow(X)
  numcolus = ncol(X)
  (m.vcol.min = matrix(vcol.min,numfilas,numcolus,byrow=TRUE))
  (m.vcol.max = matrix(vcol.max,numfilas,numcolus,byrow=TRUE))
  rango = vcol.max - vcol.min
  rango[rango == 0] = 1
  m.rango = matrix(rango,numfilas,numcolus,byrow=TRUE)
  Nueva.X = (X-m.vcol.min)/m.rango
  return(Nueva.X)
}


#' Homogeneizacion por metodo Promethee
#'
#' Normaliza una matriz de decision usando las funciones de preferencia
#' del metodo Promethee.
#'
#' @param Xmatrizdecision Matriz numerica de decision.
#' @param v.delta.min Vector numerico con los valores de indiferencia (q)
#'   para cada criterio.
#' @param v.delta.max Vector numerico con los valores de preferencia (p)
#'   para cada criterio.
#'
#' @return Matriz numerica homogeneizada con flujos netos por criterio.
#'
#' @examples
#' Xmat <- multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#' multicriterio.homogeneizacion.promethee(Xmat, c(30,3,4), c(120,12,10))
#'
#' @export
multicriterio.homogeneizacion.promethee = function(Xmatrizdecision,v.delta.min,v.delta.max) {
  validar_matriz_numerica(Xmatrizdecision)
  if (length(v.delta.min) != ncol(Xmatrizdecision) ||
      length(v.delta.max) != ncol(Xmatrizdecision) ||
      any(!is.finite(v.delta.min)) || any(!is.finite(v.delta.max)) ||
      any(v.delta.min < 0) || any(v.delta.max <= v.delta.min)) {
    stop("Los umbrales deben cumplir 0 <= delta.min < delta.max por criterio.",
         call. = FALSE)
  }
  X.matriz = Xmatrizdecision
  X.nueva = c();
  for (i in (1:dim(X.matriz)[2]) ) {
    X.rel.bor = func_matriz.relacion.borrosa(X.matriz[,i],v.delta.min[i],v.delta.max[i]);
    v.flujo.neto = func_calculo.flujo.neto(X.rel.bor);
    X.nueva = cbind(X.nueva,v.flujo.neto)
  }
  colnames(X.nueva) = colnames(Xmatrizdecision)
  rownames(X.nueva) = rownames(Xmatrizdecision)
  return(X.nueva);
}


## Funciones auxiliares para homogeneizacion Promethee ----

#' @noRd
func_matriz.relacion.borrosa = function(vcolumna,delta.min,delta.max) {
  X.rel.bor = matrix(NA,length(vcolumna),length(vcolumna));
  for (i in 1:length(vcolumna)) {
    for (j in 1:length(vcolumna)) {
      X.rel.bor[i,j] = vcolumna[i] - vcolumna[j];
      if (X.rel.bor[i,j] <= delta.min) {
        X.rel.bor[i,j] = 0;
      } else if (X.rel.bor[i,j] >= delta.max) {
        X.rel.bor[i,j] = 1;
      } else {
        X.rel.bor[i,j] = (X.rel.bor[i,j]-delta.min)/(delta.max - delta.min);
      }
    }
  }
  return(X.rel.bor)
}

#' @noRd
func_calculo.flujo.neto = function(X.matriz) {
  flujo.saliente = rowSums(X.matriz);
  flujo.entrante = colSums(X.matriz);
  flujo.neto = flujo.saliente-flujo.entrante;
  return(flujo.neto)
}


## 3. Metodo AHP -----

#' Chequeo de matriz de comparaciones AHP
#'
#' Verifica si una matriz de comparaciones por pares cumple la propiedad
#' de reciprocidad (a_ij = 1/a_ji).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada numerica de comparaciones por pares.
#'
#' @return Logico. TRUE si la matriz cumple la reciprocidad, FALSE en caso contrario.
#'
#' @examples
#' C <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("A","B"))
#' multicriterio.metodoAHP.chequeo.matrizcomparaciones(C)
#'
#' @export
multicriterio.metodoAHP.chequeo.matrizcomparaciones = function(Xmatrizvaloraciones) {
validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)
dset = Xmatrizvaloraciones
nitem <- ncol(dset)
if (ncol(dset) != nrow(dset)) {
  message("Not a square matrix")
  return(FALSE)
}
recip <- 0
for (i in 1:nitem) {
  for (j in 1:nitem) {
    if (dset[i, j] < 1/dset[j, i] * 0.99 || dset[i, j] >
          1/dset[j, i] * 1.01) {
      recip <- 1
    }
  }
}
if (recip == 1) {
  message("Please check a_{ij} = 1/a_{ij}")
  return(FALSE)
}
 return(TRUE)
}


#' AHP - Variante 1: Autovector del mayor autovalor
#'
#' Calcula las valoraciones AHP usando el metodo del autovector asociado
#' al mayor autovalor de la matriz de comparaciones.
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Xmat}{Matriz de comparaciones original}
#'   \item{autovalor}{Mayor autovalor (lambda)}
#'   \item{suma.autovector}{Suma del autovector}
#'   \item{normaeuclidea.autovector}{Norma euclidea del autovector}
#'   \item{valoraciones.ahp}{Vector de prioridades relativas (valoraciones finales)}
#'   \item{valoraciones.ahp.ordenadas}{Valoraciones ordenadas de mayor a menor}
#'   \item{CI.coef.inconsistencia}{Coeficiente de inconsistencia CI}
#'   \item{RI.coef.inconsistencia}{Coeficiente de inconsistencia relativo RI}
#'   \item{consistencia}{Mensaje sobre la consistencia}
#'   \item{tablaresumen}{Tabla resumen con autovalores y prioridades}
#' }
#'
#' @examples
#' vector <- c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 4)
#' resultado <- multicriterio.metodoAHP.variante1.autovectormayorautovalor(Xmat)
#' resultado$valoraciones.ahp
#'
#' @seealso \code{\link{multicriterio.metodoAHP.variante2.mediageometrica}},
#'   \code{\link{multicriterio.metodoAHP.variante3.basico}},
#'   \code{\link{multicriterio.metodoAHP.coef.inconsistencia}}
#' @export
multicriterio.metodoAHP.variante1.autovectormayorautovalor = function(Xmatrizvaloraciones) {

  validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)

  C.valora = Xmatrizvaloraciones

  autovalores.C = eigen(C.valora,symmetric=FALSE)
  autova.C = Re(autovalores.C$values[Im(autovalores.C$values)==0])
  autovec.C = Re(autovalores.C$vectors[,Im(autovalores.C$values)==0])
  icual = which.max(autova.C)
  lambda = autova.C[icual]
  if (is.null(dim(autovec.C))) {
   autovector.v = autovec.C
  } else {
   autovector.v = autovec.C[,icual]
  }
  (valoraciones.finales.ahp = autovector.v/sum(autovector.v))
  names(valoraciones.finales.ahp) = colnames(C.valora)
  prioridades.relativas = valoraciones.finales.ahp
  tablaresumen = C.valora
  tablaresumen = cbind(tablaresumen,c(lambda,rep(NA,nrow(C.valora)-1)),autovector.v,prioridades.relativas)
  tablaresumen = rbind(tablaresumen,c(rep(NA,ncol(C.valora)+1),sum(autovector.v),NA))

  (mm = dim(C.valora)[1]);
  (CI.coef.inconsistencia = (lambda-mm)/(mm-1));
  (CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24,1.32,1.41));
  (RI.coef.inconsistencia = CI.coef.inconsistencia/CA.coefs.inconsistencia.aleatorio[mm])
  if (mm <= 2) {
   texto='Consistencia aceptable';
  } else {
  if (RI.coef.inconsistencia < 0.1) {
    texto = 'Consistencia aceptable'
  } else {
    texto = 'Consistencia no aceptable. Revisar las comparaciones de criterios por pares'
  }
  }
   salida = list()
   salida$Xmat = C.valora
   salida$autovalor = lambda
   salida$suma.autovector = sum(autovector.v)
   salida$normaeuclidea.autovector = sqrt(sum(autovector.v^2))
   salida$valoraciones.ahp = valoraciones.finales.ahp
   salida$valoraciones.ahp.ordenadas = sort(valoraciones.finales.ahp,decreasing = TRUE)
   salida$CI.coef.inconsistencia = CI.coef.inconsistencia
   salida$RI.coef.inconsistencia = RI.coef.inconsistencia
   salida$consistencia = texto
   salida$tablaresumen = tablaresumen
   return(salida)
}


#' Funcion auxiliar: media geometrica
#'
#' Calcula la media geometrica de un vector numerico.
#'
#' @param vector Vector numerico positivo.
#'
#' @return Valor numerico con la media geometrica.
#'
#' @examples
#' func_media.geometrica(c(2, 4, 8))
#'
#' @export
func_media.geometrica = function(vector) {
  return( prod(vector)^(1/length(vector)) )
}


#' AHP - Variante 2: Media geometrica
#'
#' Calcula las valoraciones AHP usando el metodo de la media geometrica
#' (minimos cuadrados de logaritmos).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Xmat}{Matriz de comparaciones original}
#'   \item{mediasgeometricas}{Medias geometricas por fila}
#'   \item{suma.mediasgeometricas}{Suma de las medias geometricas}
#'   \item{valoraciones.ahp}{Vector de prioridades relativas}
#'   \item{valoraciones.ahp.ordenadas}{Valoraciones ordenadas}
#'   \item{tablaresumen}{Tabla resumen}
#' }
#'
#' @examples
#' vector <- c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 4)
#' resultado <- multicriterio.metodoAHP.variante2.mediageometrica(Xmat)
#' resultado$valoraciones.ahp
#'
#' @seealso \code{\link{multicriterio.metodoAHP.variante1.autovectormayorautovalor}},
#'   \code{\link{multicriterio.metodoAHP.variante3.basico}},
#'   \code{\link{multicriterio.metodoAHP.coef.inconsistencia}}
#' @export
multicriterio.metodoAHP.variante2.mediageometrica = function(Xmatrizvaloraciones) {

  validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)

  C.valora = Xmatrizvaloraciones

  (mgeoms = apply(C.valora,1,func_media.geometrica))
  (valoraciones.finales.ahp = mgeoms/sum(mgeoms))
  prioridades.relativas = valoraciones.finales.ahp
  tablaresumen = C.valora
  tablaresumen = cbind(tablaresumen,mgeoms,prioridades.relativas)
  tablaresumen = rbind(tablaresumen,c(rep(NA,ncol(C.valora)),sum(mgeoms),NA))

  salida = list()
  salida$Xmat = C.valora
  salida$mediasgeometricas = mgeoms
  salida$suma.mediasgeometricas = sum(mgeoms)
  salida$valoraciones.ahp = valoraciones.finales.ahp
  salida$valoraciones.ahp.ordenadas = sort(valoraciones.finales.ahp,decreasing = TRUE)
  salida$tablaresumen = tablaresumen
  return(salida)

}


#' AHP - Variante 3: Sintetizacion basica
#'
#' Calcula las valoraciones AHP usando el metodo de sintetizacion basico
#' (normalizacion por columnas y media por filas).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Xmat}{Matriz de comparaciones original}
#'   \item{sumacolumnas}{Suma de cada columna}
#'   \item{Xmat.normalizada}{Matriz normalizada}
#'   \item{valoraciones.ahp}{Vector de prioridades relativas}
#'   \item{valoraciones.ahp.ordenadas}{Valoraciones ordenadas}
#'   \item{tablaresumen}{Tabla resumen}
#' }
#'
#' @examples
#' vector <- c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 4)
#' resultado <- multicriterio.metodoAHP.variante3.basico(Xmat)
#' resultado$valoraciones.ahp
#'
#' @seealso \code{\link{multicriterio.metodoAHP.variante1.autovectormayorautovalor}},
#'   \code{\link{multicriterio.metodoAHP.variante2.mediageometrica}},
#'   \code{\link{multicriterio.metodoAHP.coef.inconsistencia}}
#' @export
multicriterio.metodoAHP.variante3.basico = function(Xmatrizvaloraciones) {

  validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)

  C.valora = Xmatrizvaloraciones

  (suma.columnas = colSums(C.valora));
  (m.suma.columnas = matrix(suma.columnas,length(suma.columnas),
                            length(suma.columnas),byrow=TRUE))
  (C.valora.normalizada = C.valora/m.suma.columnas)
  (prioridades.relativas = rowMeans(C.valora.normalizada))

  tablaresumen = C.valora
  tablaresumen = cbind(tablaresumen,C.valora.normalizada,prioridades.relativas)
  tablaresumen = rbind(tablaresumen,c(suma.columnas,rep(NA,ncol(C.valora)+1)))

  salida = list()
  salida$Xmat = C.valora
  salida$sumacolumnas = suma.columnas
  salida$Xmat.normalizada = C.valora.normalizada
  salida$valoraciones.ahp = prioridades.relativas
  salida$valoraciones.ahp.ordenadas = sort(prioridades.relativas,decreasing = TRUE)
  salida$tablaresumen = tablaresumen
  return(salida)

}


#' AHP - Pesos globales de alternativas
#'
#' Calcula los pesos globales de las alternativas combinando los pesos de
#' criterios con las valoraciones locales de cada alternativa.
#'
#' @param pesos.criterios Vector numerico con las prioridades de los criterios.
#' @param matriz.pesos.locales Matriz numerica donde las filas son los pesos
#'   locales de las alternativas para cada criterio.
#'
#' @return Vector numerico con los pesos globales de cada alternativa.
#'
#' @examples
#' pesos_cri <- c(0.6, 0.4)
#' pesos_locales <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
#' multicriterio.metodoAHP.pesosglobales(pesos_cri, pesos_locales)
#'
#' @seealso \code{\link{multicriterio.metodoAHP.pesosglobales_entabla}},
#'   \code{\link{multicriterio.metodoAHP.variante3.completo}}
#' @export
multicriterio.metodoAHP.pesosglobales = function(pesos.criterios,matriz.pesos.locales) {
  validar_matriz_numerica(matriz.pesos.locales)
  validar_pesos(pesos.criterios, nrow(matriz.pesos.locales))
  vector1 = as.numeric(pesos.criterios%*%matriz.pesos.locales)
  names(vector1) = colnames(matriz.pesos.locales)
  return(vector1)
}


#' AHP - Pesos globales en tabla
#'
#' Calcula los pesos globales y los presenta en formato de tabla para
#' su visualizacion.
#'
#' @param pesos.criterios Vector numerico con las prioridades de los criterios.
#' @param matriz.pesos.locales Matriz numerica con los pesos locales.
#'
#' @return Matriz numerica con los pesos locales, ponderacion de criterios
#'   y ponderadores globales.
#'
#' @examples
#' pesos_cri <- c(0.6, 0.4)
#' pesos_locales <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
#' multicriterio.metodoAHP.pesosglobales_entabla(pesos_cri, pesos_locales)
#'
#' @seealso \code{\link{multicriterio.metodoAHP.pesosglobales}},
#'   \code{\link{multicriterio.metodoAHP.variante3.completo}}
#' @export
multicriterio.metodoAHP.pesosglobales_entabla = function(pesos.criterios,
                                                         matriz.pesos.locales) {
  vector1 = as.numeric(pesos.criterios%*%matriz.pesos.locales)

  tablafinal = rbind(t(matriz.pesos.locales),pesos.criterios)
  tablafinal = cbind(tablafinal,c(vector1,NA))
  rownames(tablafinal)[nrow(tablafinal)] = "Ponder.Criterios"
  colnames(tablafinal)[ncol(tablafinal)] = "Ponderadores Globales"

  return(tablafinal)
}


#' AHP - Coeficiente de inconsistencia
#'
#' Calcula el coeficiente de inconsistencia de una matriz de comparaciones
#' por pares segun el metodo AHP.
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{lambda}{Mayor autovalor}
#'   \item{m}{Dimension de la matriz}
#'   \item{CI.coef.inconsistencia}{Coeficiente de inconsistencia}
#'   \item{CA.aleatorio}{Coeficiente aleatorio para la dimension m}
#'   \item{RI.coef.inconsistencia}{Coeficiente relativo de inconsistencia}
#'   \item{mensaje}{Mensaje sobre la consistencia aceptable}
#' }
#'
#' @examples
#' vector <- c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 4)
#' multicriterio.metodoAHP.coef.inconsistencia(Xmat)
#'
#' @export
multicriterio.metodoAHP.coef.inconsistencia = function(Xmatrizvaloraciones) {
  validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)
  C.valora = Xmatrizvaloraciones
  autovalores.C = eigen(C.valora,symmetric=FALSE);
  autova.C = Re(autovalores.C$values[Im(autovalores.C$values)==0]);
  icual = which.max(autova.C);
  lambda = autova.C[icual];
  mm = dim(C.valora)[1];
  CI.coef.inconsistencia = (lambda-mm)/(mm-1);
  CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49);
  RI.coef.inconsistencia = CI.coef.inconsistencia/CA.coefs.inconsistencia.aleatorio[mm];
  if (mm <= 2) {
    texto='Consistencia aceptable';
  } else {
    if (RI.coef.inconsistencia < 0.1) {
      texto='Consistencia aceptable';
    } else {
      texto='Consistencia no aceptable. Revisar las comparaciones de criterios por pares';
    }
  }
  salida = list()
  salida$lambda = lambda
  salida$m = mm
  salida$CI.coef.inconsistencia = CI.coef.inconsistencia
  salida$CA.aleatorio = CA.coefs.inconsistencia.aleatorio[mm]
  salida$RI.coef.inconsistencia = RI.coef.inconsistencia
  salida$mensaje = texto
  return(salida);
}


#' AHP Variante 3 con coeficiente de inconsistencia
#'
#' Calcula las valoraciones AHP (variante 3) junto con su coeficiente
#' de inconsistencia.
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{lambda}{Mayor autovalor}
#'   \item{m}{Dimension de la matriz}
#'   \item{CI.coef.inconsistencia}{Coeficiente de inconsistencia}
#'   \item{CA.aleatorio}{Coeficiente aleatorio}
#'   \item{RI.coef.inconsistencia}{Coeficiente relativo}
#'   \item{mensaje}{Mensaje sobre la consistencia}
#'   \item{tablaresumen}{Tabla resumen con calculos detallados}
#' }
#'
#' @examples
#' vector <- c(1,3,6,1/3,1,3,1/6,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 3)
#' multicriterio.metodoAHP.variante3.basico.coef.inconsistencia(Xmat)
#'
#' @export
multicriterio.metodoAHP.variante3.basico.coef.inconsistencia = function(Xmatrizvaloraciones) {

  validar_matriz_numerica(Xmatrizvaloraciones, cuadrada = TRUE, positiva = TRUE)

  C.valora = Xmatrizvaloraciones

  (suma.columnas = colSums(C.valora));
  (m.suma.columnas = matrix(suma.columnas,length(suma.columnas),
                            length(suma.columnas),byrow=TRUE))
  (C.valora.normalizada = C.valora/m.suma.columnas)
  (prioridades.relativas = rowMeans(C.valora.normalizada))

  paso1 = C.valora %*% prioridades.relativas
  paso2 = paso1 / prioridades.relativas
  lambda = mean(paso2)
  tablaresumen = C.valora
  tablaresumen = cbind(tablaresumen,paso1,paso2)
  tablaresumen = rbind(tablaresumen,c(prioridades.relativas,NA,lambda))
  mm = dim(C.valora)[1];
  CI.coef.inconsistencia = (lambda-mm)/(mm-1);
  CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49);
  RI.coef.inconsistencia = CI.coef.inconsistencia/CA.coefs.inconsistencia.aleatorio[mm];
  if (mm <= 2) {
    texto='Consistencia aceptable';
  } else {
    if (RI.coef.inconsistencia < 0.1) {
      texto='Consistencia aceptable';
    } else {
      texto='Consistencia no aceptable. Revisar las comparaciones de criterios por pares';
    }
  }

  salida = list()
  salida$lambda = lambda
  salida$m = mm
  salida$CI.coef.inconsistencia = CI.coef.inconsistencia
  salida$CA.aleatorio = CA.coefs.inconsistencia.aleatorio[mm]
  salida$RI.coef.inconsistencia = RI.coef.inconsistencia
  salida$mensaje = texto
  salida$tablaresumen = tablaresumen
  return(salida);

}


#' AHP - Metodo completo multinivel
#'
#' Aplica el metodo AHP completo con dos niveles de jerarquia: criterios
#' y alternativas para cada criterio.
#'
#' @param Xmatriznivel1 Matriz cuadrada de comparaciones de criterios (nivel 1).
#' @param Xmatriznivel2 Array tridimensional con las matrices de comparaciones
#'   de alternativas para cada criterio (nivel 2).
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{pesos.nivel1}{Resultado de AHP para el nivel 1}
#'   \item{l.pesos.nivel2}{Lista de resultados AHP para cada criterio del nivel 2}
#'   \item{inconsistencia.nivel1}{Inconsistencia del nivel 1}
#'   \item{l.inconsistencia.nivel2}{Lista de inconsistencias del nivel 2}
#'   \item{pesos.globales}{Pesos globales de las alternativas}
#'   \item{pesos.globales_entabla}{Tabla con pesos globales formateados}
#' }
#'
#' @examples
#' # Ejemplo 1: dos criterios y dos alternativas (nombres autogenerados)
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("Coste","Calidad"))
#' mat_alt1 <- multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1), 2, c("A","B"))
#' mat_alt2 <- multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1), 2, c("A","B"))
#' mat_nivel2 <- array(NA, dim=c(2,2,2))
#' mat_nivel2[,,1] <- mat_alt1
#' mat_nivel2[,,2] <- mat_alt2
#' resultado <- multicriterio.metodoAHP.variante3.completo(mat_cri, mat_nivel2)
#' resultado$pesos.globales
#' resultado$pesos.globales_entabla
#'
#' # Ejemplo 2: tres criterios y tres alternativas con nombres propios en el array
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1,2,3,1/2,1,4,1/3,1/4,1), 3,
#'                                                 c("C1","C2","C3"))
#' m1 <- multicriterio.crea.matrizvaloraciones(c(1,2,4,1/2,1,3,1/4,1/3,1), 3,
#'                                            c("X","Y","Z"))
#' m2 <- multicriterio.crea.matrizvaloraciones(c(1,3,2,1/3,1,1/2,1/2,2,1), 3,
#'                                            c("X","Y","Z"))
#' m3 <- multicriterio.crea.matrizvaloraciones(c(1,1/2,3,2,1,4,1/3,1/4,1), 3,
#'                                            c("X","Y","Z"))
#' mat_nivel2 <- array(NA, dim=c(3,3,3),
#'                     dimnames=list(c("X","Y","Z"), c("X","Y","Z"), c("C1","C2","C3")))
#' mat_nivel2[,,1] <- m1
#' mat_nivel2[,,2] <- m2
#' mat_nivel2[,,3] <- m3
#' resultado <- multicriterio.metodoAHP.variante3.completo(mat_cri, mat_nivel2)
#' rownames(resultado$pesos.globales_entabla)
#'
#' # Ejemplo 3: consultar la inconsistencia de cada nivel
#' resultado$inconsistencia.nivel1$mensaje
#' sapply(resultado$l.inconsistencia.nivel2, function(x) x$mensaje)
#'
#' @seealso \code{\link{multicriterio.metodoAHP.variante1.autovectormayorautovalor}},
#'   \code{\link{multicriterio.metodoAHP.variante2.mediageometrica}},
#'   \code{\link{multicriterio.metodoAHP.variante3.basico}},
#'   \code{\link{multicriterio.metodoAHP.coef.inconsistencia}},
#'   \code{\link{multicriterio.metodoAHP.pesosglobales}},
#'   \code{\link{multicriterio.metodoAHP.pesosglobales_entabla}}
#' @export
multicriterio.metodoAHP.variante3.completo = function(Xmatriznivel1,Xmatriznivel2) {

  inconsistencia.nivel1 = multicriterio.metodoAHP.coef.inconsistencia(Xmatriznivel1)
  pesos.nivel1 = multicriterio.metodoAHP.variante3.basico(Xmatriznivel1)
  num.cri.nivel1 = dim(Xmatriznivel2)[3]
  num.alt.nivel2 = dim(Xmatriznivel2)[1]
  l.inconsistencia.nivel2 = vector("list",num.cri.nivel1)
  l.pesos.nivel2 = vector("list",num.cri.nivel1)
  m.pesos.nivel2 = matrix(NA,nrow = num.cri.nivel1,ncol = num.alt.nivel2)
  # Nombres de las alternativas (nivel 2), tomados de forma consistente. Se
  # priorizan los dimnames del array 3D; si no los tiene, los de la primera
  # matriz de criterio; y si tampoco hay, se autogeneran (a1, a2, ...) para que
  # la tabla devuelta siempre muestre nombres de alternativas. Al asignar
  # rebanadas de un array (Xmatriznivel2[, , i] <- mat) R no propaga los
  # rownames de las matrices individuales al array, por lo que usarlos directo
  # dentro del bucle devolve NULL y las alternativas quedaban sin nombre.
  nombres.alternativas = dimnames(Xmatriznivel2)[[1]]
  if (is.null(nombres.alternativas)) {
    nombres.alternativas = rownames(Xmatriznivel2[,,1])
  }
  if (is.null(nombres.alternativas) ||
      length(nombres.alternativas) != num.alt.nivel2 ||
      any(is.na(nombres.alternativas)) ||
      any(nombres.alternativas == "")) {
    nombres.alternativas = paste0("a", 1:num.alt.nivel2)
  }
  for (i in 1:num.cri.nivel1) {
    Xmatriz = Xmatriznivel2[,,i]
    l.inconsistencia.nivel2[[i]] = multicriterio.metodoAHP.coef.inconsistencia(Xmatriz)
    l.pesos.nivel2[[i]] = multicriterio.metodoAHP.variante3.basico(Xmatriz)
    m.pesos.nivel2[i,] = l.pesos.nivel2[[i]]$valoraciones.ahp
  }
  colnames(m.pesos.nivel2) = nombres.alternativas
  rownames(m.pesos.nivel2) = rownames(Xmatriznivel1)
  pesos.globales = multicriterio.metodoAHP.pesosglobales(pesos.nivel1$valoraciones.ahp,m.pesos.nivel2)
  pesos.globales_entabla = multicriterio.metodoAHP.pesosglobales_entabla(pesos.nivel1$valoraciones.ahp,
                                                                         m.pesos.nivel2)
  names(pesos.globales) = nombres.alternativas
  salida = list()
  salida$pesos.nivel1 = pesos.nivel1
  salida$l.pesos.nivel2 = l.pesos.nivel2
  salida$inconsistencia.nivel1 = inconsistencia.nivel1
  salida$l.inconsistencia.nivel2 = l.inconsistencia.nivel2
  salida$pesos.globales = pesos.globales
  salida$pesos.globales_entabla = pesos.globales_entabla
  # Elemento "Calculos" con los resultados intermedios (evita salidas largas
  # cuando se genera un informe). Ordena las alternativas de mayor a menor
  # peso global.
  ord = order(pesos.globales, decreasing = TRUE)
  salida$Calculos = list(
    pesos.nivel1 = pesos.nivel1,
    l.pesos.nivel2 = l.pesos.nivel2,
    m.pesos.nivel2 = m.pesos.nivel2,
    nombres.alternativas = nombres.alternativas,
    nombres.criterios = rownames(Xmatriznivel1),
    pesos.globales.ordenados = pesos.globales[ord],
    orden.alternativas = ord
  )
  return(salida)
}


#' Tablas tinytable para AHP variante 3 completo
#'
#' Genera tablas de tipo \code{tinytable} a partir del resultado de
#' \code{\link{multicriterio.metodoAHP.variante3.completo}}, listas para
#' renderizar en documentos Quarto (HTML, Typst/PDF o LaTeX).
#'
#' @param resultado Objeto lista devuelto por
#'   \code{\link{multicriterio.metodoAHP.variante3.completo}}.
#' @param ordenar Logico. Si \code{TRUE} (por defecto), las alternativas de
#'   la tabla de pesos globales se ordenan de mayor a menor peso.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla_pesos_globales}{Tabla tinytable con alternativa y peso
#'     global, ordenada de mayor a menor peso.}
#'   \item{tabla_pesos_locales}{Tabla tinytable con la matriz de pesos
#'     locales: filas = criterios, columnas = alternativas.}
#'   \item{tabla_inconsistencia}{Tabla tinytable con el coeficiente de
#'     inconsistencia (RI) de cada nivel (nivel 1 y nivel 2 por criterio).}
#' }
#'
#' @examples
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2,
#'                                                 c("Coste","Calidad"))
#' mat_alt1 <- multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1), 2,
#'                                                  c("A","B"))
#' mat_alt2 <- multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1), 2,
#'                                                  c("A","B"))
#' mat_nivel2 <- array(NA, dim=c(2,2,2))
#' mat_nivel2[,,1] <- mat_alt1
#' mat_nivel2[,,2] <- mat_alt2
#' resultado <- multicriterio.metodoAHP.variante3.completo(mat_cri, mat_nivel2)
#' tablas <- multicriterio.metodoAHP.variante3.completo_tinytable(resultado)
#' tablas$tabla_pesos_globales
#'
#' @seealso \code{\link{multicriterio.metodoAHP.variante3.completo}},
#'   \code{\link{multicriterio.metodoAHP.pesosglobales_formattable}}
#' @export
multicriterio.metodoAHP.variante3.completo_tinytable = function(resultado,
                                                                 ordenar = TRUE) {
  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package \"tinytable\" needed for this function. Please install it with install.packages(\"tinytable\").", call. = FALSE)
  }

  # Tabla de pesos globales (ordenados si se pide)
  pesos = if (ordenar) resultado$Calculos$pesos.globales.ordenados
          else resultado$pesos.globales
  alt_names = names(pesos)
  df_globales = data.frame(
    Alternativa = alt_names,
    Peso_Global = round(as.numeric(pesos), 4),
    stringsAsFactors = FALSE
  )
  if (ordenar) {
    df_globales = df_globales[order(df_globales$Peso_Global,
                                    decreasing = TRUE), , drop = FALSE]
    rownames(df_globales) = NULL
  }
  tabla_globales = tinytable::tt(df_globales,
                                 caption = "Pesos globales AHP (alternativas)")

  # Tabla de pesos locales (criterios x alternativas)
  m_locales = resultado$Calculos$m.pesos.nivel2
  cri_names = rownames(m_locales)
  alt_names_loc = colnames(m_locales)
  df_locales = data.frame(
    Criterio = cri_names,
    round(m_locales, 4),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  colnames(df_locales)[-1] = alt_names_loc
  tabla_locales = tinytable::tt(df_locales,
                                caption = "Pesos locales AHP por criterio")

  # Tabla de inconsistencia
  inc_n1 = if (!is.null(resultado$inconsistencia.nivel1$RI.coef.inconsistencia))
             resultado$inconsistencia.nivel1$RI.coef.inconsistencia else NA_real_
  inc_n2 = sapply(resultado$l.inconsistencia.nivel2, function(x) {
    if (!is.null(x$RI.coef.inconsistencia)) x$RI.coef.inconsistencia else NA_real_
  })
  df_inc = data.frame(
    Nivel = c("Nivel 1 (criterios)", paste0("Nivel 2 - ",
                                          resultado$Calculos$nombres.criterios)),
    RI = round(c(inc_n1, inc_n2), 4),
    Consistencia = ifelse(is.na(c(inc_n1, inc_n2)) |
                            c(inc_n1, inc_n2) < 0.10,
                          "Aceptable", "No aceptable"),
    stringsAsFactors = FALSE
  )
  tabla_inc = tinytable::tt(df_inc,
                            caption = "Coeficiente de inconsistencia AHP (RI)")

  list(tabla_pesos_globales = tabla_globales,
       tabla_pesos_locales = tabla_locales,
       tabla_inconsistencia = tabla_inc)
}


#' Exportar tabla formattable a imagen
#'
#' Exporta una tabla formattable a un fichero de imagen PNG.
#'
#' @param f Objeto formattable.
#' @param file Character string. Ruta del fichero de salida.
#' @param width Character string. Ancho del widget HTML.
#' @param height Altura del widget HTML.
#' @param background Character string. Color de fondo.
#' @param delay Numerico. Retraso en segundos para la captura.
#'
#' @return None. Crea un fichero PNG como efecto secundario.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Alternativa = c("A1", "A2"), Valor = c(0.65, 0.35))
#' ft <- formattable::formattable(df)
#' export_formattable(ft, file = tempfile(fileext = ".png"))
#' }
#'
#' @export
export_formattable <- function(f, file, width = "100%", height = NULL,
                               background = "white", delay = 0.2)
{
  if (!requireNamespace("formattable", quietly = TRUE)) {
    stop("Package \"formattable\" needed for this function. Please install it with install.packages(\"formattable\").", call. = FALSE)
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package \"htmltools\" needed for this function. Please install it with install.packages(\"htmltools\").", call. = FALSE)
  }
  if (!requireNamespace("webshot", quietly = TRUE)) {
    stop("Package \"webshot\" needed for this function. Please install it with install.packages(\"webshot\").", call. = FALSE)
  }
    w <- formattable::as.htmlwidget(f, width = width, height = height)
    path <- htmltools::html_print(w, background = background, viewer = NULL)
    url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot::webshot(url,
                     file = file,
                     selector = ".formattable_widget",
                     delay = delay)
}


#' AHP - Pesos globales con formattable
#'
#' Calcula los pesos globales AHP y genera tablas formattable para
#' su visualizacion en HTML.
#'
#' @param tb.nivel01 Matriz de comparaciones de criterios (nivel 1).
#' @param l_tb.nivel02 Lista de matrices de comparaciones de alternativas
#'   para cada criterio.
#' @param que.variante Numerico. Variante AHP a usar (1=autovector, 2=geometrica,
#'   3=basica). Por defecto 1.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tb01sal}{Tabla formattable de interpretacion/aportaciones}
#'   \item{tb02sal}{Tabla formattable de pesos globales con inconsistencias}
#'   \item{pglobales_mej}{Matriz de pesos globales mejorada}
#'   \item{tb01}{DataFrame de interpretacion}
#'   \item{tb02}{DataFrame de pesos con inconsistencias}
#' }
#'
#' @examples
#' \dontrun{
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1, 2, 1/2, 1), 2,
#'                                                 c("Rendimiento", "Riesgo"))
#' mat_alt1 <- multicriterio.crea.matrizvaloraciones(c(1, 3, 1/3, 1), 2,
#'                                                  c("A", "B"))
#' mat_alt2 <- multicriterio.crea.matrizvaloraciones(c(1, 1/2, 2, 1), 2,
#'                                                  c("A", "B"))
#' resultado <- multicriterio.metodoAHP.pesosglobales_formattable(
#'   mat_cri, list(mat_alt1, mat_alt2), que.variante = 1
#' )
#' resultado$tb01sal
#' }
#'
#' @export
multicriterio.metodoAHP.pesosglobales_formattable = function(tb.nivel01, l_tb.nivel02,
                                                             que.variante = 1) {

  if (!requireNamespace("formattable", quietly = TRUE)) {
    stop("Package \"formattable\" needed for this function. Please install it with install.packages(\"formattable\").", call. = FALSE)
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
    if (que.variante == 1) {
        pesos.nivel01 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tb.nivel01)
        l_pesos.nivel02 = lapply(1:length(l_tb.nivel02),
                                 function(x)
                                     multicriterio.metodoAHP.variante1.autovectormayorautovalor(l_tb.nivel02[[x]])
        )
    } else if (que.variante == 2) {
        pesos.nivel01 = multicriterio.metodoAHP.variante2.mediageometrica(tb.nivel01)
        l_pesos.nivel02 = lapply(1:length(l_tb.nivel02),
                                 function(x)
                                     multicriterio.metodoAHP.variante2.mediageometrica(l_tb.nivel02[[x]])
        )
    } else if (que.variante == 3) {
        pesos.nivel01 = multicriterio.metodoAHP.variante3.basico(tb.nivel01)
        l_pesos.nivel02 = lapply(1:length(l_tb.nivel02),
                                 function(x)
                                     multicriterio.metodoAHP.variante3.basico(l_tb.nivel02[[x]])
        )
    }

    v_incons_nivel01 = multicriterio.metodoAHP.coef.inconsistencia(tb.nivel01)
    v_incons_nivel01 = ifelse(is.na(v_incons_nivel01$RI.coef.inconsistencia),
                              0,v_incons_nivel01$RI.coef.inconsistencia)
    v_incons_nivel02 = sapply(1:length(l_tb.nivel02),
                              function(x) {
                                  r1 = multicriterio.metodoAHP.coef.inconsistencia(l_tb.nivel02[[x]])
                                  r2 = ifelse(is.na(r1$RI.coef.inconsistencia),0,r1$RI.coef.inconsistencia)
                                  return(r2)
                              }
    )
    v_incons_todos = c(v_incons_nivel01, v_incons_nivel02)
    m_pesos.nivel02 = do.call(rbind,
                              lapply(1:length(l_pesos.nivel02), function(x) l_pesos.nivel02[[x]]$valoraciones.ahp)
    )

    pglobales = multicriterio.metodoAHP.pesosglobales_entabla(pesos.nivel01$valoraciones.ahp,
                                                              m_pesos.nivel02)

    ind_ord_r = c(nrow(pglobales), 1:(nrow(pglobales)-1) )
    ind_ord_c = c(ncol(pglobales), 1:(ncol(pglobales)-1) )
    pglobales_mej = t(pglobales[ind_ord_r,ind_ord_c])
    pglobales_mej[1,1] = 1

    pglobales_mej2 = pglobales_mej
    pglobales_mej2[1,-1] = NA

    formatea_NA = formattable::formatter("span",
                            style = x ~ formattable::style(color = ifelse(is.na(x), "white", x)))
    formatea_inconsistencia <- formattable::formatter("span",
                                         x ~ formattable::icontext(ifelse(x>=0.10,"remove","ok"),paste0(round(100*x,2),"%")),
                                         style = x ~ formattable::style(display = "block",
                                                           padding = "0 4px",
                                                           `border-radius` = "4px",
                                                           color = ifelse(x>=0.10,"red","green"),
                                                           background = ifelse(x>=0.10, "yellow",formattable::csscolor(formattable::gradient(as.numeric(x),"lightyellow","#ffff70"))))
    )

    dtpg04mej3 = as.data.frame(pglobales_mej2)
    dtpg04mej3 = cbind("Pesos Locales AHP" = rownames(dtpg04mej3),
                       dtpg04mej3)
    rownames(dtpg04mej3) =  c()
    dtpg04mej3[1,1] = "Peso Total"
    colnames(dtpg04mej3)[2] = "Criterios/Alternativas"
    dtpg04mej3$Inconsistencia = v_incons_todos

    tb02sal = (dtpg04mej3) |>
        formattable::formattable(
            align = c("l",rep("r",(ncol(dtpg04mej3)-1))),
            list(
                formattable::area(col = -1) ~ formattable::percent,
                formattable::area(row = 1,col = -1) ~ formatea_NA,
                formattable::area(col = 2, row = -1) ~ formattable::color_tile("#ecf0ec","#B4C3B4"),
                formattable::area(row = 2:nrow(dtpg04mej3), col = -c(1:2,ncol(dtpg04mej3))) ~ formattable::color_tile("#ddd9dd","#786778"),
                formattable::area(col = 1) ~ formattable::formatter("span",
                                          style = x ~ formattable::style("font-weight" = "bold")),
                formattable::area(col = ncol(dtpg04mej3)) ~ formatea_inconsistencia
            )
        )

    dtpg04mej10 = as.data.frame(pglobales_mej2)
    dtpg04mej11 = dtpg04mej10[,1] * dtpg04mej10[,-1]
    dtpg04mej11v = colSums(dtpg04mej11, na.rm = TRUE)
    dtpg04mej11[1,] = dtpg04mej11v
    dtpg04mej10[,-1] = dtpg04mej11
    dtpg04mej12 = dtpg04mej10
    dtpg04mej12b = cbind("Interpretacion-Aportaciones AHP" = rownames(dtpg04mej12),
                         dtpg04mej12)
    rownames(dtpg04mej12b) =  c()
    colnames(dtpg04mej12b)[2] = "Ponderaciones Criterios"

    tb01sal = (dtpg04mej12b) |>
        formattable::formattable(
            align = c("l",rep("r",(ncol(dtpg04mej12b)-1))),
            list(
                formattable::area(col = -1) ~ formattable::percent,
                formattable::area(col = 2, row = -1) ~ formattable::color_tile("#ecf0ec","#B4C3B4"),
                formattable::area(col = -(1:2)) ~ formattable::color_tile("#ddd9dd","#786778"),
                formattable::area(col = 1) ~ formattable::formatter("span",
                                          style = x ~ formattable::style("font-weight" = "bold"))
            )
        )

    res = list(
        tb01sal = tb01sal,
        tb02sal = tb02sal,
        pglobales_mej = pglobales_mej,
        tb01 = dtpg04mej12b,
        tb02 = dtpg04mej3
    )
    return(res)
}


## Multicriterio ELECTRE ----

#' Metodo ELECTRE I
#'
#' Aplica el metodo ELECTRE I para identificar el nucleo (alternativas no
#' dominadas) de un problema de decision multicriterio.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param nivel.concordancia.minimo.alpha Numerico. Umbral minimo de concordancia.
#' @param no.se.compensan Vector numerico con los umbrales de discordancia
#'   para cada criterio.
#' @param que.alternativas Vector numerico o logico para filtrar alternativas.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{datos}{Matriz original con pesos y umbrales}
#'   \item{alpha}{Nivel de concordancia minimo}
#'   \item{Imas}{Array de relaciones de preferencia positiva}
#'   \item{Iigual}{Array de relaciones de indiferencia}
#'   \item{Imenos}{Array de relaciones de preferencia negativa}
#'   \item{ind.concordancia}{Matriz de indices de concordancia}
#'   \item{ind.concordancia.gorro}{Matriz de cocientes de concordancia}
#'   \item{test.concordancia}{Matriz de test de concordancia}
#'   \item{test.discordancia}{Matriz de test de discordancia}
#'   \item{relacion.dominante}{Relacion de dominancia agregada}
#'   \item{nucleo_aprox}{Indices del nucleo aproximado}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' pesos <- c(0.3, 0.2, 0.4, 0.1)
#' multicriterio.metodoELECTRE_I(tabdec, pesos, 0.7, c(15, Inf, 2, Inf))
#'
#' @seealso \code{\link{multicriterio.metodoelectre_varlibro}}
#' @export
multicriterio.metodoELECTRE_I = function(tabdecs.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan,que.alternativas=TRUE) {

  validar_matriz_numerica(tabdecs.X)
  validar_pesos(pesos.criterios, ncol(tabdecs.X))
  if (length(nivel.concordancia.minimo.alpha) != 1 ||
      !is.finite(nivel.concordancia.minimo.alpha) ||
      nivel.concordancia.minimo.alpha < 0 || nivel.concordancia.minimo.alpha > 1) {
    stop("El umbral de concordancia debe estar entre 0 y 1.", call. = FALSE)
  }
  if (length(no.se.compensan) != ncol(tabdecs.X) ||
      any(is.na(no.se.compensan)) || any(no.se.compensan < 0)) {
    stop("Los umbrales de discordancia deben ser no negativos.", call. = FALSE)
  }

  if (!is.logical(que.alternativas)) {
    tabdec.X = tabdecs.X[que.alternativas,];
  } else {
    tabdec.X = tabdecs.X;
  }

  num.alt = dim(tabdec.X)[1];
  num.cri = dim(tabdec.X)[2];
  Imas = array(FALSE,dim=c(num.alt,num.alt,num.cri));
  Iigual = array(FALSE,dim=c(num.alt,num.alt,num.cri));
  Imenos = array(FALSE,dim=c(num.alt,num.alt,num.cri));
  rownames(Imas)=rownames(tabdec.X);colnames(Imas)=rownames(tabdec.X);
  rownames(Iigual)=rownames(tabdec.X);colnames(Iigual)=rownames(tabdec.X);
  rownames(Imenos)=rownames(tabdec.X);colnames(Imenos)=rownames(tabdec.X);
  dimnames(Imas)[[3]] = colnames(tabdec.X)
  dimnames(Iigual)[[3]] = colnames(tabdec.X)
  dimnames(Imenos)[[3]] = colnames(tabdec.X)

  for (j in 1:num.alt) {
    for (k in 1:num.alt) {
      for (i in 1:num.cri) {
        if (tabdec.X[j,i] > tabdec.X[k,i]) {
          Imas[j,k,i] = TRUE;
        } else if (tabdec.X[j,i] < tabdec.X[k,i]) {
          Imenos[j,k,i] = TRUE;
        } else {
          Iigual[j,k,i] = TRUE;
        }
      }
    }
  }

  ind.concordancia = array(NA,dim=c(num.alt,num.alt));
  ind.concordancia.gorro = array(NA,dim=c(num.alt,num.alt));
  rownames(ind.concordancia)=rownames(tabdec.X);colnames(ind.concordancia)=rownames(tabdec.X);
  rownames(ind.concordancia.gorro)=rownames(tabdec.X);colnames(ind.concordancia.gorro)=rownames(tabdec.X);

  suma.pesos.criterios = sum(pesos.criterios);
  for (j in 1:num.alt) {
    for (k in 1:num.alt) {
      suma1 = sum(pesos.criterios[Imas[j,k,]]);
      suma2 = sum(pesos.criterios[Iigual[j,k,]]);
      ind.concordancia[j,k] = (suma1+suma2)/suma.pesos.criterios;
      suma3 = sum(pesos.criterios[Imenos[j,k,]]);
      ind.concordancia.gorro[j,k] = suma1/suma3;
    }
  }

  test.concordancia = array(FALSE,dim=c(num.alt,num.alt));
  for (j in 1:num.alt) {
    for (k in 1:num.alt) {
      if (!is.nan(ind.concordancia.gorro[j,k])) {
        if ( (ind.concordancia[j,k] >= nivel.concordancia.minimo.alpha) & (ind.concordancia.gorro[j,k] >= 1) ) {
          test.concordancia[j,k] = TRUE;
        }
      }
    }
  }
  test.discordancia = array(FALSE,dim=c(num.alt,num.alt));
  for (j in 1:num.alt) {
    for (k in 1:num.alt) {
      suma1 = 0;
      suma2 = 0;
      for (i in 1:num.cri) {
        if (Imenos[j,k,i]) {
          suma2 = suma2 + 1;
          suma1 = suma1 + ( (tabdec.X[k,i]-tabdec.X[j,i]) < no.se.compensan[i] );
        }
      }
      if ((suma2==0) | ((suma1!=0) & (suma1 == suma2))) {
        test.discordancia[j,k] = TRUE;
      }
    }
  }

  relacion.dominante2 = matrix(as.logical(test.concordancia * test.discordancia),num.alt,num.alt)
  rownames(test.concordancia)=rownames(tabdec.X);colnames(test.concordancia)=rownames(tabdec.X);
  rownames(test.discordancia)=rownames(tabdec.X);colnames(test.discordancia)=rownames(tabdec.X);
  rownames(relacion.dominante2)=rownames(tabdec.X);colnames(relacion.dominante2)=rownames(tabdec.X);

  suma = apply(relacion.dominante2,2,sum,na.rm=TRUE)
  nucleo = which(suma==0)

  resultado = list();
  resultado$datos = rbind(tabdec.X,pesos.criterios,no.se.compensan)
  resultado$alpha = nivel.concordancia.minimo.alpha
  resultado$Imas =Imas;
  resultado$Iigual =Iigual;
  resultado$Imenos =Imenos;
  resultado$ind.concordancia = ind.concordancia;
  resultado$ind.concordancia.gorro = ind.concordancia.gorro;
  resultado$test.concordancia = test.concordancia;
  resultado$test.discordancia = test.discordancia;
  resultado$relacion.dominante = relacion.dominante2;
  resultado$nucleo_aprox = nucleo

  return(resultado);
}


#' Metodo ELECTRE I (variante del libro de Cordoba)
#'
#' Implementacion alternativa del metodo ELECTRE I segun la metodologia
#' descrita en "Metodologia para la toma de decisiones" de Miguel Cordoba Bueno.
#'
#' @param Mvalor Matriz numerica de valoracion (alternativas x criterios).
#'   Los criterios de coste deben estar en negativo.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param umbral.c Numerico o NA. Umbral de concordancia. Si es NA, se usa la media.
#' @param umbral.d Numerico o NA. Umbral de discordancia. Si es NA, se usa la media.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Mvalor}{Matriz de valoracion original}
#'   \item{pesos.criterios}{Pesos utilizados}
#'   \item{umbral.c.medio}{Umbral de concordancia medio}
#'   \item{umbral.d.medio}{Umbral de discordancia medio}
#'   \item{umbral.c}{Umbral de concordancia final}
#'   \item{umbral.d}{Umbral de discordancia final}
#'   \item{Mindconcor}{Matriz de concordancia}
#'   \item{Minddiscor}{Matriz de discordancia}
#'   \item{Mindconcor_conc}{Matriz de concordancia test}
#'   \item{Minddiscor_disc}{Matriz de discordancia test}
#'   \item{Minddominancia_agregada}{Relacion de dominancia agregada}
#'   \item{nucleo_aprox}{Indices del nucleo aproximado}
#' }
#'
#' @examples
#' Mvalor <- matrix(c(8,25,20,10,75,5,12,35,17,25,100,7), nrow=2, ncol=6, byrow=TRUE)
#' multicriterio.metodoelectre_varlibro(Mvalor, c(0.15,0.25,0.20,0.10,0.20,0.10))
#'
#' @seealso \code{\link{multicriterio.metodoELECTRE_I}}
#' @export
multicriterio.metodoelectre_varlibro <- function (Mvalor, pesos.criterios,umbral.c=NA,umbral.d=NA) {
  num.alt = nrow(Mvalor)
  num.cri = ncol(Mvalor)
  Mindconcor = matrix(NA,nrow = num.alt,ncol=num.alt)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      if (i!=j) {
        pesoij = 0
        for (k in 1:num.cri) {
          if (Mvalor[i,k]>Mvalor[j,k]) {
            pesoij = pesoij + pesos.criterios[k]
          } else if (Mvalor[i,k]==Mvalor[j,k]) {
            pesoij = pesoij + 0.5 * pesos.criterios[k]
          }
        }
        Mindconcor[i,j] = pesoij
      }
    }
  }
  rownames(Mindconcor) = colnames(Mindconcor) = rownames(Mvalor)

  Mdecnormalizada = matrix(NA,nrow=num.alt,ncol=num.cri)
  rownames(Mdecnormalizada) = rownames(Mvalor)
  colnames(Mdecnormalizada) = colnames(Mvalor)
  min.col = apply(Mvalor,2,min)
  max.col = apply(Mvalor,2,max)
  Mdecnormalizada = abs(t(t(Mvalor)/(max.col-min.col)))
  Mdecnormalizada.pond = t(t(Mdecnormalizada) * pesos.criterios)

  Minddiscor = matrix(NA,nrow = num.alt,ncol=num.alt)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      if (i!=j) {
         max.num = 0
         max.den = 0
        for (k in 1:num.cri) {
          if (Mvalor[i,k]<Mvalor[j,k]) {
            max.num = max(max.num, abs(Mdecnormalizada.pond[i,k]-Mdecnormalizada.pond[j,k]))
          }
          max.den = max(max.den, abs(Mdecnormalizada.pond[i,k]-Mdecnormalizada.pond[j,k]))
        }
        Minddiscor[i,j] = max.num/max.den
      }
    }
  }
  rownames(Minddiscor) = colnames(Minddiscor) = rownames(Mvalor)

  umbral.c.medio = mean(Mindconcor,na.rm=TRUE)
  umbral.d.medio = mean(Minddiscor,na.rm=TRUE)

  if (is.na(umbral.c)) {
    umbral.c.temp = umbral.c.medio
  }else {
    umbral.c.temp = umbral.c
  }
  if (is.na(umbral.d)) {
    umbral.d.temp = umbral.d.medio
  }else {
    umbral.d.temp = umbral.d
  }
  Mindconcor_conc = (Mindconcor>=umbral.c.temp) * 1.0
  Minddiscor_disc = (Minddiscor<=umbral.d.temp) * 1.0
  Minddominancia_agregada = Mindconcor_conc * Minddiscor_disc
  suma = apply(Minddominancia_agregada,2,sum,na.rm=TRUE)
  nucleo = which(suma==0)

  salida = list()
  salida$Mvalor = Mvalor
  salida$pesos.criterios = pesos.criterios
  salida$umbral.c.medio = umbral.c.medio
  salida$umbral.d.medio = umbral.d.medio
  salida$umbral.c = umbral.c.temp
  salida$umbral.d = umbral.d.temp
  salida$Mindconcor = Mindconcor
  salida$Minddiscor = Minddiscor
  salida$Mindconcor_conc = Mindconcor_conc
  salida$Minddiscor_disc = Minddiscor_disc
  salida$Minddominancia_agregada = Minddominancia_agregada
  salida$nucleo_aprox = nucleo
  return(salida)
}


## Metodo Promethee ----

#' Funciones de preferencia para Promethee
#'
#' Funciones auxiliares que definen las funciones de preferencia utilizadas
#' en el metodo Promethee.
#'
#' @param vaj Numerico. Valor de la alternativa a.
#' @param vah Numerico. Valor de la alternativa h.
#' @param qi Numerico. Umbral de indiferencia.
#' @param pi Numerico. Umbral de preferencia.
#' @param si Numerico. Parametro de la funcion gaussiana.
#'
#' @return Valor numerico de la preferencia (entre 0 y 1).
#'
#' @examples
#' fpref.criterio_usual(10, 5)
#' fpref.cuasi_criterio(10, 5, qi = 3)
#' fpref.criterio_preflineal(10, 5, pi = 8)
#'
#' @export
fpref.criterio_usual = function(vaj,vah) {
  di = vaj-vah;
  if (di <= 0) {
    res = 0;
  } else {
    res = 1;
  }
  return(res);
}

#' @rdname fpref.criterio_usual
#' @export
fpref.cuasi_criterio = function(vaj,vah,qi) {
  di = vaj - vah;
  if (di <= qi) {
    res = 0;
  } else {
    res = 1;
  }
  return(res);
}

#' @rdname fpref.criterio_usual
#' @export
fpref.criterio_preflineal = function(vaj,vah,pi) {
  di = vaj - vah;
  if (di <= 0) {
    res = 0;
  } else if (di > pi) {
    res = 1;
  } else {
    res = di/pi;
  }
  return(res);
}

#' @rdname fpref.criterio_usual
#' @export
fpref.criterio_nivel = function(vaj,vah,qi,pi) {
  di = vaj - vah;
  if (di <= qi) {
    res = 0;
  } else if (di >= pi) {
    res = 1;
  } else {
    res = 0.5;
  }
  return(res);
}

#' @rdname fpref.criterio_usual
#' @export
fpref.criterio_preflineal_indif = function(vaj,vah,qi,pi) {
  di = vaj - vah;
  if (di <= qi) {
    res = 0;
  } else if (di > pi) {
    res = 1;
  } else {
    res = (di-qi)/(pi-qi);
  }
  return(res);
}

#' @rdname fpref.criterio_usual
#' @export
fpref.criterio_gaussiano = function(vaj,vah,qi,pi,si) {
  di = vaj - vah;
  if (di <= 0) {
    res = 0;
  } else {
    res = 1 - exp((-di^2)/(2*si^2));
  }
  return(res);
}

#' Funcion de preferencia selector
#'
#' Selecciona y aplica la funcion de preferencia adecuada segun el tipo indicado.
#'
#' @param cual Numerico. Tipo de funcion (1=Usual, 2=U-shape, 3=V-shape,
#'   4=Level, 5=Linear, 6=Gaussian).
#' @param vaj Numerico. Valor de la alternativa a.
#' @param vah Numerico. Valor de la alternativa h.
#' @param qi Numerico. Umbral de indiferencia. Por defecto 0.
#' @param pi Numerico. Umbral de preferencia. Por defecto 1.
#' @param si Numerico. Parametro gaussiano. Por defecto 0.5.
#'
#' @return Valor numerico de la preferencia.
#'
#' @examples
#' fpref.todas(1, 10, 5)  # Usual
#' fpref.todas(2, 10, 5, qi = 3)  # U-shape
#'
#' @export
fpref.todas = function(cual,vaj,vah,qi=0,pi=1,si=0.5) {
  if (cual == 2) {
    res = fpref.cuasi_criterio(vaj,vah,qi);
  } else if (cual==3) {
    res = fpref.criterio_preflineal(vaj,vah,pi);
  } else if (cual==4) {
    res = fpref.criterio_nivel(vaj,vah,qi,pi);
  } else if (cual==5) {
    res = fpref.criterio_preflineal_indif(vaj,vah,qi,pi);
  } else if (cual==6) {
    res = fpref.criterio_gaussiano(vaj,vah,qi,pi,si);
  } else {
    res = fpref.criterio_usual(vaj,vah);
  }
  return(res);
}


#' Indice de preferencia multicriterio
#'
#' Calcula la matriz de preferencia agregada multicriterio para todos los
#' pares de alternativas.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'   Columnas: tipo funcion, qi, pi, si.
#'
#' @return Matriz cuadrada de preferencia multicriterio.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' tab.indices <- indice.prefmulticriterio(X, pesos, fpref)
#' tab.indices
#'
#' @seealso \code{\link{fpref.todas}},
#'   \code{\link{flujo.entrante.Promethee}},
#'   \code{\link{multicriterio.metodo.promethee_i}},
#'   \code{\link{multicriterio.metodo.promethee_ii}}
#' @export
indice.prefmulticriterio = function(tabdecs.X,pesos.criterios,tab.fpref) {
  validar_matriz_numerica(tabdecs.X)
  validar_pesos(pesos.criterios, ncol(tabdecs.X))
  if (!is.data.frame(tab.fpref) && !is.matrix(tab.fpref)) {
    stop("tab.fpref debe ser una matriz o data.frame.", call. = FALSE)
  }
  if (nrow(tab.fpref) != ncol(tabdecs.X) || ncol(tab.fpref) < 4) {
    stop("tab.fpref debe tener una fila y cuatro columnas por criterio.", call. = FALSE)
  }
  num.alt = nrow(tabdecs.X);
  num.cri = ncol(tabdecs.X);
  tab.indices = matrix(NA,num.alt,num.alt);
  for (i in 1:num.alt) {
    for (h in 1:num.alt) {
      total = 0;
      for (j in 1:num.cri) {
        vai = tabdecs.X[i,j];
        vah = tabdecs.X[h,j];
        cual = tab.fpref[j,1];
        qi = tab.fpref[j,2];
        pi = tab.fpref[j,3];
        si = tab.fpref[j,4];
        total = total + pesos.criterios[j] * fpref.todas(cual,vai,vah,qi,pi,si)
      }
      tab.indices[i,h] = total;
    }
  }
  return(tab.indices);
}


#' Flujos Promethee
#'
#' Funciones para calcular flujos entrantes, salientes y netos en Promethee.
#'
#' @param tab.indices Matriz de indices de preferencia multicriterio.
#'
#' @return Vector numerico con los flujos.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' tab.indices <- indice.prefmulticriterio(X, pesos, fpref)
#' flujo.entrante.Promethee(tab.indices)
#' flujo.saliente.Promethee(tab.indices)
#' flujo.neto.Promethee(tab.indices)
#'
#' @seealso \code{\link{flujo.saliente.Promethee}},
#'   \code{\link{flujo.neto.Promethee}},
#'   \code{\link{indice.prefmulticriterio}},
#'   \code{\link{multicriterio.metodo.promethee_i}},
#'   \code{\link{multicriterio.metodo.promethee_ii}}
#' @export
flujo.entrante.Promethee = function(tab.indices) {
  res = rowSums(tab.indices);
  return(res);
}

#' @rdname flujo.entrante.Promethee
#' @export
flujo.saliente.Promethee = function(tab.indices) {
  res = colSums(tab.indices);
  return(res);
}

#' @rdname flujo.entrante.Promethee
#' @export
flujo.neto.Promethee = function(tab.indices) {
  vflujo.ent = flujo.entrante.Promethee(tab.indices);
  vflujo.sal = flujo.saliente.Promethee(tab.indices);
  res = vflujo.ent - vflujo.sal;
  return(res);
}


#' Flujos medios Promethee
#'
#' Funciones para calcular flujos medios (promediados) en Promethee.
#'
#' @param tab.indices Matriz de indices de preferencia multicriterio.
#'
#' @return Vector numerico con los flujos medios.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' tab.indices <- indice.prefmulticriterio(X, pesos, fpref)
#' flujomed.entrante.Promethee(tab.indices)
#' flujomed.saliente.Promethee(tab.indices)
#' flujomed.neto.Promethee(tab.indices)
#'
#' @seealso \code{\link{flujomed.saliente.Promethee}},
#'   \code{\link{flujomed.neto.Promethee}},
#'   \code{\link{flujo.entrante.Promethee}},
#'   \code{\link{multicriterio.metodo.promethee_i_med}},
#'   \code{\link{multicriterio.metodo.promethee_ii_med}}
#' @export
flujomed.entrante.Promethee = function(tab.indices) {
  res = rowSums(tab.indices)/(dim(tab.indices)[1]-1);
  return(res);
}

#' @rdname flujomed.entrante.Promethee
#' @export
flujomed.saliente.Promethee = function(tab.indices) {
  res = colSums(tab.indices)/(dim(tab.indices)[1]-1);
  return(res);
}

#' @rdname flujomed.entrante.Promethee
#' @export
flujomed.neto.Promethee = function(tab.indices) {
  vflujo.ent = flujomed.entrante.Promethee(tab.indices);
  vflujo.sal = flujomed.saliente.Promethee(tab.indices);
  res = vflujo.ent - vflujo.sal;
  return(res);
}


#' Metodo Promethee I
#'
#' Aplica el metodo Promethee I que establece una relacion de preferencia
#' parcial entre alternativas basada en flujos entrantes y salientes.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla.indices}{Matriz de indices de preferencia multicriterio}
#'   \item{vflujos.ent}{Vector de flujos entrantes}
#'   \item{vflujos.sal}{Vector de flujos salientes}
#'   \item{tablarelacionsupera}{Matriz de relacion superacion}
#' }
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_i(X, pesos, fpref)
#'
#' @seealso \code{\link{multicriterio.metodo.promethee_ii}},
#'   \code{\link{flujo.entrante.Promethee}},
#'   \code{\link{flujo.neto.Promethee}}
#' @export
multicriterio.metodo.promethee_i = function(tabdecs.X,pesos.criterios,tab.fpref) {

  tab.indices =  indice.prefmulticriterio(tabdecs.X,pesos.criterios,tab.fpref);
  vflujo.ent = flujo.entrante.Promethee(tab.indices);
  vflujo.sal = flujo.saliente.Promethee(tab.indices);
  colnames(tab.indices)=rownames(tab.indices) = rownames(tabdecs.X)
  names(vflujo.ent) = rownames(tabdecs.X)
  names(vflujo.sal) = rownames(tabdecs.X)
  num.alt = length(vflujo.ent);
  tab.Pthee.i = matrix(NA,num.alt,num.alt);
  colnames(tab.Pthee.i)=rownames(tab.Pthee.i) = rownames(tabdecs.X)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      smas = (vflujo.ent[i] > vflujo.ent[j]);
      imas = (vflujo.ent[i] == vflujo.ent[j]);
      smenos = (vflujo.sal[i] < vflujo.sal[j]);
      imenos = (vflujo.sal[i] == vflujo.sal[j]);
      if ((smas & smenos) | (smas & imenos) | (imas & smenos)) {
        tab.Pthee.i[i,j]=1;
      } else if (imas & imenos) {
        tab.Pthee.i[i,j]=0.5;
      } else {
        tab.Pthee.i[i,j]=0;
      }
    }
  }
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.ent=vflujo.ent
  salida$vflujos.sal=vflujo.sal
  salida$tablarelacionsupera = tab.Pthee.i
  return(salida)

}


#' Metodo Promethee II
#'
#' Aplica el metodo Promethee II que establece una clasificacion completa
#' de alternativas basada en flujos netos.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla.indices}{Matriz de indices de preferencia multicriterio}
#'   \item{vflujos.netos}{Vector de flujos netos}
#'   \item{tablarelacionsupera}{Matriz de relacion superacion}
#' }
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_ii(X, pesos, fpref)
#'
#' @seealso \code{\link{multicriterio.metodo.promethee_i}},
#'   \code{\link{flujo.neto.Promethee}},
#'   \code{\link{flujo.entrante.Promethee}}
#' @export
multicriterio.metodo.promethee_ii = function(tabdecs.X,pesos.criterios,tab.fpref) {

  tab.indices =  indice.prefmulticriterio(tabdecs.X,pesos.criterios,tab.fpref);
  vflujos.netos = flujo.neto.Promethee(tab.indices);
  colnames(tab.indices)=rownames(tab.indices) = rownames(tabdecs.X)
  names(vflujos.netos) = rownames(tabdecs.X)
  num.alt = length(vflujos.netos);
  tab.Pthee.ii = matrix(NA,num.alt,num.alt);
  colnames(tab.Pthee.ii)=rownames(tab.Pthee.ii) = rownames(tabdecs.X)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      if (vflujos.netos[i] > vflujos.netos[j]) {
        tab.Pthee.ii[i,j]=1;
      } else if (vflujos.netos[i] == vflujos.netos[j]) {
        tab.Pthee.ii[i,j]=0.5;
      } else {
        tab.Pthee.ii[i,j]=0;
      }
    }
  }
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.netos=vflujos.netos
  salida$tablarelacionsupera = tab.Pthee.ii
  return(salida)

}


#' Metodo Promethee I con flujos medios
#'
#' Variante de Promethee I que utiliza flujos medios en lugar de sumas.
#'
#' @param tabdecs.X Matriz numerica de decision.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Lista con tabla de indices, flujos medios y relacion superacion.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_i_med(X, pesos, fpref)
#'
#' @export
multicriterio.metodo.promethee_i_med = function(tabdecs.X,pesos.criterios,tab.fpref) {

  tab.indices =  indice.prefmulticriterio(tabdecs.X,pesos.criterios,tab.fpref);
  vflujo.ent = flujomed.entrante.Promethee(tab.indices);
  vflujo.sal = flujomed.saliente.Promethee(tab.indices);
  colnames(tab.indices)=rownames(tab.indices) = rownames(tabdecs.X)
  names(vflujo.ent) = rownames(tabdecs.X)
  names(vflujo.sal) = rownames(tabdecs.X)
  num.alt = length(vflujo.ent);
  tab.Pthee.i = matrix(NA,num.alt,num.alt);
  colnames(tab.Pthee.i)=rownames(tab.Pthee.i) = rownames(tabdecs.X)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      smas = (vflujo.ent[i] > vflujo.ent[j]);
      imas = (vflujo.ent[i] == vflujo.ent[j]);
      smenos = (vflujo.sal[i] < vflujo.sal[j]);
      imenos = (vflujo.sal[i] == vflujo.sal[j]);
      if ((smas & smenos) | (smas & imenos) | (imas & smenos)) {
        tab.Pthee.i[i,j]=1;
      } else {
        tab.Pthee.i[i,j]=0;
      }
    }
  }
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.ent=vflujo.ent
  salida$vflujos.sal=vflujo.sal
  salida$tablarelacionsupera = tab.Pthee.i
  return(salida)
}


#' Metodo Promethee II con flujos medios
#'
#' Variante de Promethee II que utiliza flujos medios en lugar de sumas.
#'
#' @param tabdecs.X Matriz numerica de decision.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Lista con tabla de indices, flujos netos medios y relacion superacion.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_ii_med(X, pesos, fpref)
#'
#' @export
multicriterio.metodo.promethee_ii_med = function(tabdecs.X,pesos.criterios,tab.fpref) {

  tab.indices =  indice.prefmulticriterio(tabdecs.X,pesos.criterios,tab.fpref);
  vflujos.netos = flujomed.neto.Promethee(tab.indices);
  colnames(tab.indices)=rownames(tab.indices) = rownames(tabdecs.X)
  names(vflujos.netos) = rownames(tabdecs.X)
  num.alt = length(vflujos.netos);
  tab.Pthee.ii = matrix(NA,num.alt,num.alt);
  colnames(tab.Pthee.ii)=rownames(tab.Pthee.ii) = rownames(tabdecs.X)
  for (i in 1:num.alt) {
    for (j in 1:num.alt) {
      if (vflujos.netos[i] > vflujos.netos[j]) {
        tab.Pthee.ii[i,j]=1;
      } else {
        tab.Pthee.ii[i,j]=0;
      }
    }
  }
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.netos=vflujos.netos
  salida$tablarelacionsupera = tab.Pthee.ii
  return(salida)
}


## Metodo TOPSIS ----

validar_fminmax <- function(fminmax, n) {
  if (is.null(fminmax)) {
    return(rep("max", n))
  }
  fminmax = tolower(as.character(fminmax))
  if (length(fminmax) != n || any(!fminmax %in% c("min", "max"))) {
    stop("fminmax debe ser NULL o un vector con \"min\" o \"max\" por criterio.", call. = FALSE)
  }
  return(fminmax)
}

topsis_normaliza_vector <- function(X) {
  normas = sqrt(colSums(X^2))
  if (any(normas == 0)) {
    stop("No se puede normalizar: hay criterios con todos los valores nulos.", call. = FALSE)
  }
  Y = sweep(X, 2, normas, "/")
  colnames(Y) = colnames(X)
  rownames(Y) = rownames(X)
  return(Y)
}

topsis_construye_salida <- function(metodo, X, pesos.criterios, fminmax, matriz_normalizada,
                                    ideal, antiideal, separacion_ideal, separacion_antiideal) {
  denominador = separacion_ideal + separacion_antiideal
  cerca_ideal = separacion_antiideal / denominador
  cerca_ideal[denominador == 0] = 0
  names(separacion_ideal) = rownames(X)
  names(separacion_antiideal) = rownames(X)
  names(cerca_ideal) = rownames(X)
  rango = rank(-cerca_ideal, ties.method = "first")
  tabla = data.frame(Alternativa = rownames(X),
                     Separacion_ideal = round(unname(separacion_ideal), 4),
                     Separacion_antiideal = round(unname(separacion_antiideal), 4),
                     Cerca_ideal = round(unname(cerca_ideal), 4),
                     Rango = unname(rango),
                     stringsAsFactors = FALSE)
  tabla = tabla[order(tabla$Rango), ]
  rownames(tabla) = NULL
  salida = list(metodo = metodo,
                matriz_decision = X,
                pesos = pesos.criterios,
                fminmax = fminmax,
                matriz_normalizada = matriz_normalizada,
                ideal = ideal,
                antiideal = antiideal,
                separacion_ideal = separacion_ideal,
                separacion_antiideal = separacion_antiideal,
                cerca_ideal = cerca_ideal,
                tabla = tabla)
  return(salida)
}

#' Metodo TOPSIS
#'
#' Aplica el metodo TOPSIS (Technique for Order Preference by Similarity to
#' Ideal Solution) de Hwang y Yoon: normaliza la matriz de decision por
#' columnas (normalizacion vectorial), pondera con los pesos de los criterios,
#' calcula las soluciones ideal y anti-ideal y ordena las alternativas segun
#' su cercania relativa a la solucion ideal.
#'
#' Sigue las etapas descritas en Chakraborty (2022): normalizacion vectorial,
#' matriz ponderada, soluciones ideales, distancias euclideas de separacion
#' y puntuacion global de preferencia.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param fminmax Vector de character strings opcional con "min" o "max"
#'   para cada criterio. Por defecto todos los criterios son de beneficio
#'   ("max").
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{metodo}{Character string con el nombre del metodo}
#'   \item{matriz_normalizada}{Matriz de decision normalizada}
#'   \item{ideal}{Solucion ideal (una fila por criterio ponderado)}
#'   \item{antiideal}{Solucion anti-ideal}
#'   \item{separacion_ideal}{Distancias euclideas a la solucion ideal}
#'   \item{separacion_antiideal}{Distancias euclideas a la solucion anti-ideal}
#'   \item{cerca_ideal}{Cercania relativa a la solucion ideal}
#'   \item{tabla}{Data frame con las alternativas ordenadas de mejor a peor
#'     (Alternativa, Separacion_ideal, Separacion_antiideal, Cerca_ideal,
#'     Rango)}
#' }
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' res <- multicriterio.metodo.topsis(X, c(0.5, 0.3, 0.2))
#' res$tabla
#'
#' @seealso \code{\link{multicriterio.metodo.topsis_modificado}}
#' @export
multicriterio.metodo.topsis = function(tabdecs.X,pesos.criterios,fminmax = NULL) {

  validar_matriz_numerica(tabdecs.X)
  validar_pesos(pesos.criterios, ncol(tabdecs.X))
  if (nrow(tabdecs.X) < 2) {
    stop("Se necesitan al menos dos alternativas.", call. = FALSE)
  }
  fminmax = validar_fminmax(fminmax, ncol(tabdecs.X))

  X = as.matrix(tabdecs.X)
  es_beneficio = fminmax == "max"

  Y = topsis_normaliza_vector(X)
  V = sweep(Y, 2, pesos.criterios, "*")

  ideal = antiideal = numeric(ncol(X))
  for (j in 1:ncol(X)) {
    if (es_beneficio[j]) {
      ideal[j] = max(V[, j])
      antiideal[j] = min(V[, j])
    } else {
      ideal[j] = min(V[, j])
      antiideal[j] = max(V[, j])
    }
  }

  separacion_ideal = sqrt(rowSums(sweep(V, 2, ideal, "-")^2))
  separacion_antiideal = sqrt(rowSums(sweep(V, 2, antiideal, "-")^2))

  salida = topsis_construye_salida("TOPSIS", X, pesos.criterios, fminmax, Y,
                                   ideal, antiideal,
                                   separacion_ideal, separacion_antiideal)
  return(salida)
}


#' Metodo TOPSIS modificado
#'
#' Variante del metodo TOPSIS de Deng, Yeh y Willis: a diferencia del TOPSIS
#' clasico, que pondera las valoraciones normalizadas, pondera las distancias
#' euclideas a las soluciones ideal y anti-ideal, calculadas sobre la matriz
#' normalizada sin ponderar. Con pesos iguales por criterio ambos metodos
#' producen la misma ordenacion; con pesos distintos pueden diferir.
#'
#' Sigue las etapas descritas en Chakraborty (2022): normalizacion vectorial,
#' soluciones ideales sobre la matriz normalizada, distancias euclideas
#' ponderadas y puntuacion global de preferencia.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param fminmax Vector de character strings opcional con "min" o "max"
#'   para cada criterio. Por defecto todos los criterios son de beneficio
#'   ("max").
#'
#' @return Lista con los mismos elementos que
#'   \code{\link{multicriterio.metodo.topsis}}.
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' res <- multicriterio.metodo.topsis_modificado(X, c(0.5, 0.3, 0.2))
#' res$tabla
#'
#' @seealso \code{\link{multicriterio.metodo.topsis}}
#' @export
multicriterio.metodo.topsis_modificado = function(tabdecs.X,pesos.criterios,fminmax = NULL) {

  validar_matriz_numerica(tabdecs.X)
  validar_pesos(pesos.criterios, ncol(tabdecs.X))
  if (nrow(tabdecs.X) < 2) {
    stop("Se necesitan al menos dos alternativas.", call. = FALSE)
  }
  fminmax = validar_fminmax(fminmax, ncol(tabdecs.X))

  X = as.matrix(tabdecs.X)
  es_beneficio = fminmax == "max"

  Y = topsis_normaliza_vector(X)

  ideal = antiideal = numeric(ncol(X))
  for (j in 1:ncol(X)) {
    if (es_beneficio[j]) {
      ideal[j] = max(Y[, j])
      antiideal[j] = min(Y[, j])
    } else {
      ideal[j] = min(Y[, j])
      antiideal[j] = max(Y[, j])
    }
  }

  dif_ideal = sweep(Y, 2, ideal, "-")^2
  dif_antiideal = sweep(Y, 2, antiideal, "-")^2
  separacion_ideal = sqrt(rowSums(sweep(dif_ideal, 2, pesos.criterios, "*")))
  separacion_antiideal = sqrt(rowSums(sweep(dif_antiideal, 2, pesos.criterios, "*")))

  salida = topsis_construye_salida("TOPSIS modificado", X, pesos.criterios, fminmax, Y,
                                   ideal, antiideal,
                                   separacion_ideal, separacion_antiideal)
  return(salida)
}


## Metodo axiomatico de Arrow y Raymond ----

#' Matriz de clasificacion (Arrow-Raymond)
#'
#' Calcula la matriz de clasificacion para el metodo axiomatico de
#' Arrow y Raymond.
#'
#' @param Mvalor Matriz numerica de valoracion (alternativas x criterios).
#' @param pesos Vector numerico opcional con los pesos de los criterios. Si
#'   es \code{NULL} todos los criterios cuentan lo mismo.
#'
#' @return Matriz cuadrada de clasificacion donde cada elemento indica
#'   cuantas veces la alternativa i es mejor o igual que la j (ponderado con
#'   \code{pesos} si se indica).
#'
#' @examples
#' Mvalor <- matrix(c(8,25,20,10,75,5), nrow=2, ncol=3, byrow=TRUE)
#' func_calcula_matrizclasificacion(Mvalor)
#'
#' @seealso \code{\link{multicriterio.metodoaxiomatico.ArrowRaymond}}
#' @export
func_calcula_matrizclasificacion = function(Mvalor, pesos = NULL) {
  validar_matriz_numerica(Mvalor)
  if (nrow(Mvalor) < 2) {
    stop("Se necesitan al menos dos alternativas.", call. = FALSE)
  }
  num.alter=nrow(Mvalor)
  num.crit=ncol(Mvalor)
  vpesos = rep(1,num.crit)
  if (!is.null(pesos)) {
    validar_pesos(pesos, num.crit)
    vpesos = pesos
  }
  Mclasificacion = matrix(NA,nrow=num.alter,ncol=num.alter)
  rownames(Mclasificacion) = colnames(Mclasificacion) = rownames(Mvalor)
  for (i in 1:nrow(Mvalor)) {
    for (j in 1:nrow(Mvalor)) {
      if (i!=j) {
        filai = Mvalor[i,]
        filaj = Mvalor[j,]
        mayoriquej = sum((filai>filaj)*vpesos)
        igualiquej = sum((filai==filaj)*vpesos)
        Mclasificacion[i,j] = mayoriquej + 0.5 * igualiquej
      }
    }
  }
  return(Mclasificacion)
}


#' Metodo axiomatico de Arrow y Raymond
#'
#' Aplica el metodo axiomatico de Arrow y Raymond para obtener una
#' clasificacion completa de alternativas.
#'
#' @param Mvalor Matriz numerica de valoracion (alternativas x criterios).
#' @param pesos Vector numerico opcional con los pesos de los criterios. Si
#'   es \code{NULL} todos los criterios cuentan lo mismo.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{pasos}{Lista con los resultados de cada paso iterativo}
#'   \item{alternativasordenadas}{Vector con las alternativas ordenadas
#'     de mejor a peor}
#' }
#'
#' @examples
#' Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
#'                    12, 35, 17, 25, 100, 7), nrow = 2, ncol = 6, byrow = TRUE)
#' rownames(Mvalor) <- c("A1", "A2")
#' resultado <- multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor)
#' resultado$alternativasordenadas
#'
#' @seealso \code{\link{func_calcula_matrizclasificacion}}
#' @export
multicriterio.metodoaxiomatico.ArrowRaymond = function(Mvalor, pesos = NULL) {

  validar_matriz_numerica(Mvalor)
  if (nrow(Mvalor) < 2) {
    stop("Se necesitan al menos dos alternativas.", call. = FALSE)
  }
  if (!is.null(pesos)) {
    validar_pesos(pesos, ncol(Mvalor))
  }
  num.alter = nrow(Mvalor)
  salida = vector("list",num.alter-1)
  salidatemp = list()
  Mvalor_red = Mvalor
  alternativas.salen = c()
  for (i in 1:(num.alter-1)) {
     Mclasificacion_red = func_calcula_matrizclasificacion(Mvalor_red,pesos)
     nombres.alternativas = rownames(Mclasificacion_red)
     max.filas = apply(Mclasificacion_red,1,max,na.rm=TRUE)
     inds.orden = order(max.filas)
     salidatemp$Mclasificacion = Mclasificacion_red
     salidatemp$max.filas = max.filas
     salidatemp$indices.ordenados = inds.orden
     salidatemp$alternativa.sale = nombres.alternativas[inds.orden[1]]
     if (i<num.alter) {
       Mvalor_red = Mvalor_red[-inds.orden[1],]
     }
     salida[[i]] = salidatemp
     alternativas.salen = c(alternativas.salen,nombres.alternativas[inds.orden[1]])
  }
  alternativas.salen = c(alternativas.salen,nombres.alternativas[inds.orden[2]])
  salidas = list()
  salidas$pasos = salida
  salidas$alternativasordenadas = alternativas.salen[length(alternativas.salen):1]
 return(salidas)

}


#' Metodo del indicador de media ponderada
#'
#' Aplica el metodo del indicador de media ponderada: homogeneiza la matriz
#' de valoracion con el metodo de Nadir, pondera las valoraciones
#' homogeneizadas con los pesos de los criterios y obtiene la puntuacion de
#' cada alternativa como la suma de sus valoraciones ponderadas.
#'
#' @param Mvalor Matriz numerica de valoracion (alternativas x criterios).
#' @param pesos Vector numerico opcional con los pesos de los criterios. Si
#'   es \code{NULL} se usan pesos uniformes \code{1/ncriterios}.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Puntuaciones}{Vector con la puntuacion de cada alternativa}
#'   \item{Rangos_Alternativas}{Vector con el rango de cada alternativa}
#'   \item{Alternativas_Ordenadas}{Vector con las alternativas ordenadas de
#'     mejor a peor}
#'   \item{Mvalor_transf_ponderada}{Matriz de valoraciones homogeneizadas y
#'     ponderadas}
#' }
#'
#' @examples
#' Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
#'                    12, 35, 17, 25, 100, 7), nrow = 2, ncol = 6, byrow = TRUE)
#' rownames(Mvalor) <- c("A1", "A2")
#' resultado <- multicriterio.metodo.mediaPonderada(Mvalor, c(1, 1, 1, 1, 1, 1))
#' resultado$Alternativas_Ordenadas
#'
#' @export
multicriterio.metodo.mediaPonderada = function(Mvalor, pesos = NULL) {

  validar_matriz_numerica(Mvalor)
  num.crit = ncol(Mvalor)
  vpesos = rep(1,num.crit)/num.crit
  if (!is.null(pesos)) {
    validar_pesos(pesos, num.crit)
    vpesos = pesos
  }

  Mvalor_transf = multicriterio.homogeneizacion.nadir(Mvalor)*100
  Mvalor_transf_pond = t( t(Mvalor_transf)*vpesos )
  Puntuaciones = rowSums(Mvalor_transf_pond)
  Puntuaciones_ordenadas = sort(Puntuaciones, decreasing = TRUE)
  Alternativas_ordenadas = names(Puntuaciones_ordenadas)
  Alternativas_Rangos = rank(-Puntuaciones)

  return(list(
    Puntuaciones = Puntuaciones,
    Rangos_Alternativas = Alternativas_Rangos,
    Alternativas_Ordenadas = Alternativas_ordenadas,
    Mvalor_transf_ponderada = Mvalor_transf_pond
  ))

}
