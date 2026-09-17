suppressWarnings(library(formattable))
suppressWarnings(library(htmltools))
suppressWarnings(library(webshot))

## FUNCIONES MÉTODOS MULTICRITERIOS

## Nota: mayor utilidad (mayor valor) mejor


#### Funciones de creación de matrices ----

#' Crear matriz de valoraciones (diagonal completa)
#'
#' Crea una matriz cuadrada de valoraciones donde cada elemento indica la
#' relación de preferencia entre alternativas.
#'
#' @param vector_matporfilas Vector numérico con los valores de la matriz
#'   rellenable por filas.
#' @param numalternativas Numérico. Número de alternativas (filas y columnas).
#' @param v.nombres.alternativas Vector de character strings opcional con los
#'   nombres de las alternativas.
#'
#' @return Matriz cuadrada numérica con las valoraciones y nombres asignados.
#'
#' @examples
#' multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("Rendimiento","Riesgo"))
#' multicriterio.crea.matrizvaloraciones(c(1,1,1,0,1,0,1,0,0,1), 3)
#'
#' @export
multicriterio.crea.matrizvaloraciones = function(vector_matporfilas,numalternativas=4,
                                                 v.nombres.alternativas=NULL) {
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
#' Crea una matriz cuadrada de valoraciones a partir únicamente de los
#' valores de la diagonal superior. Los valores inferiores se calculan
#' como el inverso.
#'
#' @param vector_valoraciones_diagsup Vector numérico con los valores de la
#'   diagonal superior (sin incluir la diagonal de unos).
#' @param numalternativas Numérico. Número de alternativas.
#' @param v.nombres.alternativas Vector de character strings opcional con los
#'   nombres de las alternativas.
#'
#' @return Matriz cuadrada numérica completa con las valoraciones.
#'
#' @examples
#' v_val <- c(5, 3, 7, 3, 3, 6, 9, 1, 3, 1)
#' multicriterio.crea.matrizvaloraciones_mej(v_val, numalternativas = 5)
#'
#' @export
multicriterio.crea.matrizvaloraciones_mej = function(vector_valoraciones_diagsup,numalternativas=4,v.nombres.alternativas = NULL) {

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


#' Crear matriz de decisión
#'
#' Crea una matriz de decisión donde las filas son alternativas y las
#' columnas son criterios.
#'
#' @param vector_matporfilas Vector numérico con los valores de la matriz
#'   rellenable por filas.
#' @param numalternativas Numérico. Número de alternativas (filas).
#' @param numcriterios Numérico. Número de criterios (columnas).
#' @param v.nombresalt Vector de character strings opcional con nombres de alternativas.
#' @param v.nombrescri Vector de character strings opcional con nombres de criterios.
#'
#' @return Matriz numérica con los datos de decisión.
#'
#' @examples
#' multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#'
#' @export
multicriterio.crea.matrizdecision = function(vector_matporfilas,numalternativas=3,
                                    numcriterios=4,v.nombresalt=NULL,v.nombrescri=NULL) {

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


## Métodos de construcción de funciones de valor o utilidad ------

#' Construcción de función de utilidad maximales
#'
#' Construye una función de utilidad a partir de una matriz de valoraciones
#' usando el método de maximales (no borrosos).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de valoraciones entre alternativas.
#'
#' @return Vector numérico con los valores de utilidad de cada alternativa,
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


#' Construcción de función de utilidad con estructura borrosa
#'
#' Construye una función de utilidad a partir de una matriz de valoraciones
#' usando el método de flujo neto (estructura borrosa).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de valoraciones entre alternativas.
#'
#' @return Vector numérico con los flujos netos de cada alternativa (valores
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


## Homogeneización tabla decisión ------

#' Homogeneización por método Nadir
#'
#' Normaliza una matriz de decisión usando el método de escalado min-max
#' (Nadir), transformando todos los valores al rango [0, 1].
#'
#' @param Xmatrizdecision Matriz numérica de decisión (alternativas x criterios).
#'
#' @return Matriz numérica homogeneizada con valores en [0, 1].
#'
#' @examples
#' Xmat <- multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#' multicriterio.homogeneizacion.nadir(Xmat)
#'
#' @export
multicriterio.homogeneizacion.nadir = function(Xmatrizdecision) {
  X = Xmatrizdecision
  (vcol.max = apply(X,2,max))
  (vcol.min = apply(X,2,min))
  numfilas = nrow(X)
  numcolus = ncol(X)
  (m.vcol.min = matrix(vcol.min,numfilas,numcolus,byrow=T))
  (m.vcol.max = matrix(vcol.max,numfilas,numcolus,byrow=T))
  Nueva.X = (X-m.vcol.min)/(m.vcol.max - m.vcol.min)
  return(Nueva.X)
}


#' Homogeneización por método Promethee
#'
#' Normaliza una matriz de decisión usando las funciones de preferencia
#' del método Promethee.
#'
#' @param Xmatrizdecision Matriz numérica de decisión.
#' @param v.delta.min Vector numérico con los valores de indiferencia (q)
#'   para cada criterio.
#' @param v.delta.max Vector numérico con los valores de preferencia (p)
#'   para cada criterio.
#'
#' @return Matriz numérica homogeneizada con flujos netos por criterio.
#'
#' @examples
#' Xmat <- multicriterio.crea.matrizdecision(c(100,15,7,40,50,200), 2, 3)
#' multicriterio.homogeneizacion.promethee(Xmat, c(30,3,4), c(120,12,10))
#'
#' @export
multicriterio.homogeneizacion.promethee = function(Xmatrizdecision,v.delta.min,v.delta.max) {
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


## Funciones auxiliares para homogeneización Promethee ----

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


## 3. Método AHP -----

#' Chequeo de matriz de comparaciones AHP
#'
#' Verifica si una matriz de comparaciones por pares cumple la propiedad
#' de reciprocidad (a_ij = 1/a_ji).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada numérica de comparaciones por pares.
#'
#' @return Lógico. TRUE si la matriz cumple la reciprocidad, FALSE en caso contrario.
#'
#' @examples
#' C <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("A","B"))
#' multicriterio.metodoAHP.chequeo.matrizcomparaciones(C)
#'
#' @export
multicriterio.metodoAHP.chequeo.matrizcomparaciones = function(Xmatrizvaloraciones) {
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
#' Calcula las valoraciones AHP usando el método del autovector asociado
#' al mayor autovalor de la matriz de comparaciones.
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Xmat}{Matriz de comparaciones original}
#'   \item{autovalor}{Mayor autovalor (lambda)}
#'   \item{suma.autovector}{Suma del autovector}
#'   \item{normaeuclidea.autovector}{Norma euclídea del autovector}
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
#' @export
multicriterio.metodoAHP.variante1.autovectormayorautovalor = function(Xmatrizvaloraciones) {

  C.valora = Xmatrizvaloraciones

  autovalores.C = eigen(C.valora,symmetric=F)
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
   salida$valoraciones.ahp.ordenadas = sort(valoraciones.finales.ahp,decreasing = T)
   salida$CI.coef.inconsistencia = CI.coef.inconsistencia
   salida$RI.coef.inconsistencia = RI.coef.inconsistencia
   salida$consistencia = texto
   salida$tablaresumen = tablaresumen
   return(salida)
}


#' Función auxiliar: media geométrica
#'
#' Calcula la media geométrica de un vector numérico.
#'
#' @param vector Vector numérico positivo.
#'
#' @return Valor numérico con la media geométrica.
#'
#' @examples
#' func_media.geometrica(c(2, 4, 8))
#'
#' @export
func_media.geometrica = function(vector) {
  return( prod(vector)^(1/length(vector)) )
}


#' AHP - Variante 2: Media geométrica
#'
#' Calcula las valoraciones AHP usando el método de la media geométrica
#' (mínimos cuadrados de logaritmos).
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Xmat}{Matriz de comparaciones original}
#'   \item{mediasgeometricas}{Medias geométricas por fila}
#'   \item{suma.mediasgeometricas}{Suma de las medias geométricas}
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
#' @export
multicriterio.metodoAHP.variante2.mediageometrica = function(Xmatrizvaloraciones) {

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
  salida$valoraciones.ahp.ordenadas = sort(valoraciones.finales.ahp,decreasing = T)
  salida$tablaresumen = tablaresumen
  return(salida)

}


#' AHP - Variante 3: Sintetización básica
#'
#' Calcula las valoraciones AHP usando el método de sintetización básico
#' (normalización por columnas y media por filas).
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
#' @export
multicriterio.metodoAHP.variante3.basico = function(Xmatrizvaloraciones) {

  C.valora = Xmatrizvaloraciones

  (suma.columnas = colSums(C.valora));
  (m.suma.columnas = matrix(suma.columnas,length(suma.columnas),
                            length(suma.columnas),byrow=T))
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
  salida$valoraciones.ahp.ordenadas = sort(prioridades.relativas,decreasing = T)
  salida$tablaresumen = tablaresumen
  return(salida)

}


#' AHP - Pesos globales de alternativas
#'
#' Calcula los pesos globales de las alternativas combinando los pesos de
#' criterios con las valoraciones locales de cada alternativa.
#'
#' @param pesos.criterios Vector numérico con las prioridades de los criterios.
#' @param matriz.pesos.locales Matriz numérica donde las filas son los pesos
#'   locales de las alternativas para cada criterio.
#'
#' @return Vector numérico con los pesos globales de cada alternativa.
#'
#' @examples
#' pesos_cri <- c(0.6, 0.4)
#' pesos_locales <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
#' multicriterio.metodoAHP.pesosglobales(pesos_cri, pesos_locales)
#'
#' @export
multicriterio.metodoAHP.pesosglobales = function(pesos.criterios,matriz.pesos.locales) {
  vector1 = as.numeric(pesos.criterios%*%matriz.pesos.locales)
  names(vector1) = colnames(matriz.pesos.locales)
  return(vector1)
}


#' AHP - Pesos globales en tabla
#'
#' Calcula los pesos globales y los presenta en formato de tabla para
#' su visualización.
#'
#' @param pesos.criterios Vector numérico con las prioridades de los criterios.
#' @param matriz.pesos.locales Matriz numérica con los pesos locales.
#'
#' @return Matriz numérica con los pesos locales, ponderación de criterios
#'   y ponderadores globales.
#'
#' @examples
#' pesos_cri <- c(0.6, 0.4)
#' pesos_locales <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
#' multicriterio.metodoAHP.pesosglobales_entabla(pesos_cri, pesos_locales)
#'
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
#' por pares según el método AHP.
#'
#' @param Xmatrizvaloraciones Matriz cuadrada de comparaciones por pares.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{lambda}{Mayor autovalor}
#'   \item{m}{Dimensión de la matriz}
#'   \item{CI.coef.inconsistencia}{Coeficiente de inconsistencia}
#'   \item{CA.aleatorio}{Coeficiente aleatorio para la dimensión m}
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
  C.valora = Xmatrizvaloraciones
  autovalores.C = eigen(C.valora,symmetric=F);
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
#'   \item{m}{Dimensión de la matriz}
#'   \item{CI.coef.inconsistencia}{Coeficiente de inconsistencia}
#'   \item{CA.aleatorio}{Coeficiente aleatorio}
#'   \item{RI.coef.inconsistencia}{Coeficiente relativo}
#'   \item{mensaje}{Mensaje sobre la consistencia}
#'   \item{tablaresumen}{Tabla resumen con cálculos detallados}
#' }
#'
#' @examples
#' vector <- c(1,3,6,1/3,1,3,1/6,1/3,1)
#' Xmat <- multicriterio.crea.matrizvaloraciones(vector, 3)
#' multicriterio.metodoAHP.variante3.basico.coef.inconsistencia(Xmat)
#'
#' @export
multicriterio.metodoAHP.variante3.basico.coef.inconsistencia = function(Xmatrizvaloraciones) {

  C.valora = Xmatrizvaloraciones

  (suma.columnas = colSums(C.valora));
  (m.suma.columnas = matrix(suma.columnas,length(suma.columnas),
                            length(suma.columnas),byrow=T))
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


#' AHP - Método completo multinivel
#'
#' Aplica el método AHP completo con dos niveles de jerarquía: criterios
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
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("R","Riesgo"))
#' mat_alt1 <- multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1), 2, c("A","B"))
#' mat_alt2 <- multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1), 2, c("A","B"))
#' mat_nivel2 <- array(NA, dim=c(2,2,2))
#' mat_nivel2[,,1] <- mat_alt1
#' mat_nivel2[,,2] <- mat_alt2
#' resultado <- multicriterio.metodoAHP.variante3.completo(mat_cri, mat_nivel2)
#' resultado$pesos.globales
#'
#' @export
multicriterio.metodoAHP.variante3.completo = function(Xmatriznivel1,Xmatriznivel2) {

  inconsistencia.nivel1 = multicriterio.metodoAHP.coef.inconsistencia(Xmatriznivel1)
  pesos.nivel1 = multicriterio.metodoAHP.variante3.basico(Xmatriznivel1)
  num.cri.nivel1 = dim(Xmatriznivel2)[3]
  num.alt.nivel2 = dim(Xmatriznivel2)[1]
  l.inconsistencia.nivel2 = vector("list",num.cri.nivel1)
  l.pesos.nivel2 = vector("list",num.alt.nivel2)
  m.pesos.nivel2 = matrix(NA,nrow = num.cri.nivel1,ncol = num.alt.nivel2)
  for (i in 1:num.cri.nivel1) {
    Xmatriz = Xmatriznivel2[,,i]
    l.inconsistencia.nivel2[[i]] = multicriterio.metodoAHP.coef.inconsistencia(Xmatriz)
    l.pesos.nivel2[[i]] = multicriterio.metodoAHP.variante3.basico(Xmatriz)
    m.pesos.nivel2[i,] = l.pesos.nivel2[[i]]$valoraciones.ahp
  }
  colnames(m.pesos.nivel2) = rownames(Xmatriz)
  rownames(m.pesos.nivel2) = rownames(Xmatriznivel1)
  pesos.globales = multicriterio.metodoAHP.pesosglobales(pesos.nivel1$valoraciones.ahp,m.pesos.nivel2)
  pesos.globales_entabla = multicriterio.metodoAHP.pesosglobales_entabla(pesos.nivel1$valoraciones.ahp,
                                                                         m.pesos.nivel2)
  names(pesos.globales) = rownames(Xmatriz)
  salida = list()
  salida$pesos.nivel1 = pesos.nivel1
  salida$l.pesos.nivel2 = l.pesos.nivel2
  salida$inconsistencia.nivel1 = inconsistencia.nivel1
  salida$l.inconsistencia.nivel2 = l.inconsistencia.nivel2
  salida$pesos.globales = pesos.globales
  salida$pesos.globales_entabla = pesos.globales_entabla
  return(salida)
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
#' @param delay Numérico. Retraso en segundos para la captura.
#'
#' @return None. Crea un fichero PNG como efecto secundario.
#'
#' @examples
#' \dontrun{
#' export_formattable(mi_tabla, file = "tabla.png")
#' }
#'
#' @export
export_formattable <- function(f, file, width = "100%", height = NULL,
                               background = "white", delay = 0.2)
{
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
#' su visualización en HTML.
#'
#' @param tb.nivel01 Matriz de comparaciones de criterios (nivel 1).
#' @param l_tb.nivel02 Lista de matrices de comparaciones de alternativas
#'   para cada criterio.
#' @param que.variante Numérico. Variante AHP a usar (1=autovector, 2=geométrica,
#'   3=básica). Por defecto 1.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tb01sal}{Tabla formattable de interpretación/aportaciones}
#'   \item{tb02sal}{Tabla formattable de pesos globales con inconsistencias}
#'   \item{pglobales_mej}{Matriz de pesos globales mejorada}
#'   \item{tb01}{DataFrame de interpretación}
#'   \item{tb02}{DataFrame de pesos con inconsistencias}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- multicriterio.metodoAHP.pesosglobales_formattable(
#'   mat_cri, list(mat_alt1, mat_alt2), que.variante = 1
#' )
#' resultado$tb01sal
#' }
#'
#' @export
multicriterio.metodoAHP.pesosglobales_formattable = function(tb.nivel01, l_tb.nivel02,
                                                             que.variante = 1) {

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

    formatea_NA = formatter("span",
                            style = x ~ style(color = ifelse(is.na(x), "white", x)))
    formatea_inconsistencia <- formatter("span",
                                         x ~ icontext(ifelse(x>=0.10,"remove","ok"),paste0(round(100*x,2),"%")),
                                         style = x ~ style(display = "block",
                                                           padding = "0 4px",
                                                           `border-radius` = "4px",
                                                           color = ifelse(x>=0.10,"red","green"),
                                                           background = ifelse(x>=0.10, "yellow",csscolor(gradient(as.numeric(x),"lightyellow","#ffff70"))))
    )

    dtpg04mej3 = as.data.frame(pglobales_mej2)
    dtpg04mej3 = cbind("Pesos Locales AHP" = rownames(dtpg04mej3),
                       dtpg04mej3)
    rownames(dtpg04mej3) =  c()
    dtpg04mej3[1,1] = "Peso Total"
    colnames(dtpg04mej3)[2] = "Criterios/Alternativas"
    dtpg04mej3$Inconsistencia = v_incons_todos

    tb02sal = (dtpg04mej3) |>
        formattable(
            align = c("l",rep("r",(ncol(dtpg04mej3)-1))),
            list(
                area(col = -1) ~ percent,
                area(row = 1,col = -1) ~ formatea_NA,
                area(col = 2, row = -1) ~ color_tile("#ecf0ec","#B4C3B4"),
                area(row = 2:nrow(dtpg04mej3), col = -c(1:2,ncol(dtpg04mej3))) ~ color_tile("#ddd9dd","#786778"),
                area(col = 1) ~ formatter("span",
                                          style = x ~ style("font-weight" = "bold")),
                area(col = ncol(dtpg04mej3)) ~ formatea_inconsistencia
            )
        )

    dtpg04mej10 = as.data.frame(pglobales_mej2)
    dtpg04mej11 = dtpg04mej10[,1] * dtpg04mej10[,-1]
    dtpg04mej11v = colSums(dtpg04mej11, na.rm = T)
    dtpg04mej11[1,] = dtpg04mej11v
    dtpg04mej10[,-1] = dtpg04mej11
    dtpg04mej12 = dtpg04mej10
    dtpg04mej12b = cbind("Interpretacion-Aportaciones AHP" = rownames(dtpg04mej12),
                         dtpg04mej12)
    rownames(dtpg04mej12b) =  c()
    colnames(dtpg04mej12b)[2] = "Ponderaciones Criterios"

    tb01sal = (dtpg04mej12b) |>
        formattable(
            align = c("l",rep("r",(ncol(dtpg04mej12b)-1))),
            list(
                area(col = -1) ~ percent,
                area(col = 2, row = -1) ~ color_tile("#ecf0ec","#B4C3B4"),
                area(col = -(1:2)) ~ color_tile("#ddd9dd","#786778"),
                area(col = 1) ~ formatter("span",
                                          style = x ~ style("font-weight" = "bold"))
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

#' Método ELECTRE I
#'
#' Aplica el método ELECTRE I para identificar el núcleo (alternativas no
#' dominadas) de un problema de decisión multicriterio.
#'
#' @param tabdecs.X Matriz numérica de decisión (alternativas x criterios).
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param nivel.concordancia.minimo.alpha Numérico. Umbral mínimo de concordancia.
#' @param no.se.compensan Vector numérico con los umbrales de discordancia
#'   para cada criterio.
#' @param que.alternativas Vector numérico o lógico para filtrar alternativas.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{datos}{Matriz original con pesos y umbrales}
#'   \item{alpha}{Nivel de concordancia mínimo}
#'   \item{Imas}{Array de relaciones de preferencia positiva}
#'   \item{Iigual}{Array de relaciones de indiferencia}
#'   \item{Imenos}{Array de relaciones de preferencia negativa}
#'   \item{ind.concordancia}{Matriz de índices de concordancia}
#'   \item{ind.concordancia.gorro}{Matriz de cocientes de concordancia}
#'   \item{test.concordancia}{Matriz de test de concordancia}
#'   \item{test.discordancia}{Matriz de test de discordancia}
#'   \item{relacion.dominante}{Relación de dominancia agregada}
#'   \item{nucleo_aprox}{Índices del núcleo aproximado}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20,13,15,30,5,40,0.3,0.5,0.1,0.7,0.9,0), nrow=4, ncol=6, byrow=TRUE)
#' pesos <- c(0.3, 0.2, 0.4, 0.1)
#' multicriterio.metodoELECTRE_I(tabdec, pesos, 0.7, c(15,Inf,2,Inf))
#'
#' @export
multicriterio.metodoELECTRE_I = function(tabdecs.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan,que.alternativas=TRUE) {

  if (!is.logical(que.alternativas)) {
    tabdec.X = tabdecs.X[que.alternativas,];
  } else {
    tabdec.X = tabdecs.X;
  }

  num.alt = dim(tabdec.X)[1];
  num.cri = dim(tabdec.X)[2];
  Imas = array(F,dim=c(num.alt,num.alt,num.cri));
  Iigual = array(F,dim=c(num.alt,num.alt,num.cri));
  Imenos = array(F,dim=c(num.alt,num.alt,num.cri));
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

  test.concordancia = array(F,dim=c(num.alt,num.alt));
  for (j in 1:num.alt) {
    for (k in 1:num.alt) {
      if (!is.nan(ind.concordancia.gorro[j,k])) {
        if ( (ind.concordancia[j,k] >= nivel.concordancia.minimo.alpha) & (ind.concordancia.gorro[j,k] >= 1) ) {
          test.concordancia[j,k] = TRUE;
        }
      }
    }
  }
  test.discordancia = array(F,dim=c(num.alt,num.alt));
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

  suma = apply(relacion.dominante2,2,sum,na.rm=T)
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


#' Método ELECTRE I (variante del libro de Córdoba)
#'
#' Implementación alternativa del método ELECTRE I según la metodología
#' descrita en "Metodología para la toma de decisiones" de Miguel Córdoba Bueno.
#'
#' @param Mvalor Matriz numérica de valoración (alternativas x criterios).
#'   Los criterios de coste deben estar en negativo.
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param umbral.c Numérico o NA. Umbral de concordancia. Si es NA, se usa la media.
#' @param umbral.d Numérico o NA. Umbral de discordancia. Si es NA, se usa la media.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Mvalor}{Matriz de valoración original}
#'   \item{pesos.criterios}{Pesos utilizados}
#'   \item{umbral.c.medio}{Umbral de concordancia medio}
#'   \item{umbral.d.medio}{Umbral de discordancia medio}
#'   \item{umbral.c}{Umbral de concordancia final}
#'   \item{umbral.d}{Umbral de discordancia final}
#'   \item{Mindconcor}{Matriz de concordancia}
#'   \item{Minddiscor}{Matriz de discordancia}
#'   \item{Mindconcor_conc}{Matriz de concordancia test}
#'   \item{Minddiscor_disc}{Matriz de discordancia test}
#'   \item{Minddominancia_agregada}{Relación de dominancia agregada}
#'   \item{nucleo_aprox}{Índices del núcleo aproximado}
#' }
#'
#' @examples
#' Mvalor <- matrix(c(8,25,20,10,75,5,12,35,17,25,100,7), nrow=2, ncol=6, byrow=TRUE)
#' multicriterio.metodoelectre_varlibro(Mvalor, c(0.15,0.25,0.20,0.10,0.20,0.10))
#'
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

  umbral.c.medio = mean(Mindconcor,na.rm=T)
  umbral.d.medio = mean(Minddiscor,na.rm=T)

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
  suma = apply(Minddominancia_agregada,2,sum,na.rm=T)
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


## Método Promethee ----

#' Funciones de preferencia para Promethee
#'
#' Funciones auxiliares que definen las funciones de preferencia utilizadas
#' en el método Promethee.
#'
#' @param vaj Numérico. Valor de la alternativa a.
#' @param vah Numérico. Valor de la alternativa h.
#' @param qi Numérico. Umbral de indiferencia.
#' @param pi Numérico. Umbral de preferencia.
#' @param si Numérico. Parámetro de la función gaussiana.
#'
#' @return Valor numérico de la preferencia (entre 0 y 1).
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

#' Función de preferencia selector
#'
#' Selecciona y aplica la función de preferencia adecuada según el tipo indicado.
#'
#' @param cual Numérico. Tipo de función (1=Usual, 2=U-shape, 3=V-shape,
#'   4=Level, 5=Linear, 6=Gaussian).
#' @param vaj Numérico. Valor de la alternativa a.
#' @param vah Numérico. Valor de la alternativa h.
#' @param qi Numérico. Umbral de indiferencia. Por defecto 0.
#' @param pi Numérico. Umbral de preferencia. Por defecto 1.
#' @param si Numérico. Parámetro gaussiano. Por defecto 0.5.
#'
#' @return Valor numérico de la preferencia.
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


#' Índice de preferencia multicriterio
#'
#' Calcula la matriz de preferencia agregada multicriterio para todos los
#' pares de alternativas.
#'
#' @param tabdecs.X Matriz numérica de decisión (alternativas x criterios).
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuración de funciones de preferencia.
#'   Columnas: tipo función, qi, pi, si.
#'
#' @return Matriz cuadrada de preferencia multicriterio.
#'
#' @examples
#' # Ver ejemplos en multicriterio.metodo.promethee_i
#'
#' @export
indice.prefmulticriterio = function(tabdecs.X,pesos.criterios,tab.fpref) {
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
#' @param tab.indices Matriz de índices de preferencia multicriterio.
#'
#' @return Vector numérico con los flujos.
#'
#' @examples
#' # Ver ejemplos en multicriterio.metodo.promethee_i
#'
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
#' @param tab.indices Matriz de índices de preferencia multicriterio.
#'
#' @return Vector numérico con los flujos medios.
#'
#' @examples
#' # Ver ejemplos en multicriterio.metodo.promethee_i_med
#'
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


#' Método Promethee I
#'
#' Aplica el método Promethee I que establece una relación de preferencia
#' parcial entre alternativas basada en flujos entrantes y salientes.
#'
#' @param tabdecs.X Matriz numérica de decisión (alternativas x criterios).
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuración de funciones de preferencia.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla.indices}{Matriz de índices de preferencia multicriterio}
#'   \item{vflujos.ent}{Vector de flujos entrantes}
#'   \item{vflujos.sal}{Vector de flujos salientes}
#'   \item{tablarelacionsupera}{Matriz de relación superación}
#' }
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_i(X, pesos, fpref)
#'
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


#' Método Promethee II
#'
#' Aplica el método Promethee II que establece una clasificación completa
#' de alternativas basada en flujos netos.
#'
#' @param tabdecs.X Matriz numérica de decisión (alternativas x criterios).
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuración de funciones de preferencia.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla.indices}{Matriz de índices de preferencia multicriterio}
#'   \item{vflujos.netos}{Vector de flujos netos}
#'   \item{tablarelacionsupera}{Matriz de relación superación}
#' }
#'
#' @examples
#' X <- multicriterio.crea.matrizdecision(c(10,20,5,8,15,12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1,1,1), qi = c(0,0,0), pi = c(5,3,2), si = c(1,1,1))
#' multicriterio.metodo.promethee_ii(X, pesos, fpref)
#'
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


#' Método Promethee I con flujos medios
#'
#' Variante de Promethee I que utiliza flujos medios en lugar de sumas.
#'
#' @param tabdecs.X Matriz numérica de decisión.
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuración de funciones de preferencia.
#'
#' @return Lista con tabla de índices, flujos medios y relación superación.
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


#' Método Promethee II con flujos medios
#'
#' Variante de Promethee II que utiliza flujos medios en lugar de sumas.
#'
#' @param tabdecs.X Matriz numérica de decisión.
#' @param pesos.criterios Vector numérico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuración de funciones de preferencia.
#'
#' @return Lista con tabla de índices, flujos netos medios y relación superación.
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


## Método axiomático de Arrow y Raymond ----

#' Matriz de clasificación (Arrow-Raymond)
#'
#' Calcula la matriz de clasificación para el método axiomático de
#' Arrow y Raymond.
#'
#' @param Mvalor Matriz numérica de valoración (alternativas x criterios).
#'
#' @return Matriz cuadrada de clasificación donde cada elemento indica
#'   cuántas veces la alternativa i es mejor o igual que la j.
#'
#' @examples
#' Mvalor <- matrix(c(8,25,20,10,75,5), nrow=2, ncol=3, byrow=TRUE)
#' func_calcula_matrizclasificacion(Mvalor)
#'
#' @export
func_calcula_matrizclasificacion = function(Mvalor) {
  num.alter=nrow(Mvalor)
  num.crit=ncol(Mvalor)
  Mclasificacion = matrix(NA,nrow=num.alter,ncol=num.alter)
  rownames(Mclasificacion) = colnames(Mclasificacion) = rownames(Mvalor)
  for (i in 1:nrow(Mvalor)) {
    for (j in 1:nrow(Mvalor)) {
      if (i!=j) {
        filai = Mvalor[i,]
        filaj = Mvalor[j,]
        mayoriquej = sum(filai>filaj)
        igualiquej = sum(filai==filaj)
        Mclasificacion[i,j] = mayoriquej + 0.5 * igualiquej
      }
    }
  }
  return(Mclasificacion)
}


#' Método axiomático de Arrow y Raymond
#'
#' Aplica el método axiomático de Arrow y Raymond para obtener una
#' clasificación completa de alternativas.
#'
#' @param Mvalor Matriz numérica de valoración (alternativas x criterios).
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{pasos}{Lista con los resultados de cada paso iterativo}
#'   \item{alternativasordenadas}{Vector con las alternativas ordenadas
#'     de mejor a peor}
#' }
#'
#' @examples
#' Mvalor <- matrix(c(8,25,20,10,75,5,12,35,17,25,100,7), nrow=2, ncol=3, byrow=TRUE)
#' resultado <- multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor)
#' resultado$alternativasordenadas
#'
#' @export
multicriterio.metodoaxiomatico.ArrowRaymond = function(Mvalor) {

  num.alter = nrow(Mvalor)
  salida = vector("list",num.alter-1)
  salidatemp = list()
  Mvalor_red = Mvalor
  alternativas.salen = c()
  for (i in 1:(num.alter-1)) {
     Mclasificacion_red = func_calcula_matrizclasificacion(Mvalor_red)
     nombres.alternativas = rownames(Mclasificacion_red)
     max.filas = apply(Mclasificacion_red,1,max,na.rm=T)
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
