suppressWarnings(library(formattable))
suppressWarnings(library(htmltools))
suppressWarnings(library(webshot))

## FUNCIONES MÉTODOS MULTICRITERIOS

## Nota: mayor utilidad (mayor valor) mejor

#### multicriterio.crea.matrizvaloraciones = function(vector_matporfilas,numalternativas=4)
#### multicriterio.crea.matrizvaloraciones_mej = function(vector_valoraciones_diagsup,numalternativas=4,v.nombres.alternativas = NULL)
#### multicriterio.crea.matrizdecision = function(vector_matporfilas,numalternativas=3,numcriterios=4)

## 1. MÉTODOS DE CONSTRUCCIÓN DE FUNCIONES DE VALOR O UTILIDAD:
#### multicriterio.constfuncutilidad.maximales = function(Xmatrizvaloraciones)
#### multicriterio.constfuncutilidad.estructuraborrosa = function(Xmatrizvaloraciones)

## 2. Homogeneización de columnas de una tabla de decisión
#### multicriterio.homogeneizacion.nadir = function(Xmatrizdecision)
#### multicriterio.homogeneizacion.promethee = function(Xmatrizdecision,v.delta.min,v.delta.max)

## 3. Método AHP (función de utilidad a partir de comparaciones por pares)
#### multicriterio.metodoAHP.variante1.autovectormayorautovalor = function(Xmatrizvaloraciones)
#### multicriterio.metodoAHP.variante2.mediageometrica = function(Xmatrizvaloraciones)
#### multicriterio.metodoAHP.variante3.basico = function(Xmatrizvaloraciones)


#### multicriterio.metodoAHP.pesosglobales = function(pesos.criterios,matriz.pesos.locales)
#### multicriterio.metodoAHP.coef.inconsistencia = function(Xmatrizvaloraciones)
#### multicriterio.metodoAHP.pesosglobales_entabla = function(pesos.criterios,matriz.pesos.locales)

#### multicriterio.metodoAHP.variante3.completo = function(Xmatriznivel1,Xmatriznivel2)


## 4. Multicriterio Electre
#### multicriterio.metodoELECTRE_I = function(tabdecs.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan,que.alternativas=TRUE)

#### multicriterio.metodoelectre_varlibro <- function (Mvalor, pesos.criterios,umbral.c=NA,umbral.d=NA) {



## 5. Método Promethee I y II
# multicriterio.metodo.promethee_i = function(tabdecs.X,pesos.criterios,tab.fpref)
# multicriterio.metodo.promethee_ii = function(tabdecs.X,pesos.criterios,tab.fpref)
# multicriterio.metodo.promethee_i_med = function(tabdecs.X,pesos.criterios,tab.fpref)
# multicriterio.metodo.promethee_ii_med = function(tabdecs.X,pesos.criterios,tab.fpref)
#
# fpref.criterio_usual = function(vaj,vah)
# fpref.cuasi_criterio = function(vaj,vah,qi)
# fpref.criterio_preflineal = function(vaj,vah,pi)
# fpref.criterio_nivel = function(vaj,vah,qi,pi)
# fpref.criterio_preflineal_indif = function(vaj,vah,qi,pi)
# fpref.criterio_gaussiano = function(vaj,vah,qi,pi,si)
# fpref.todas = function(cual,vaj,vah,qi=0,pi=1,si=0.5)
#
# indice.prefmulticriterio = function(tabdecs.X,pesos.criterios,tab.fpref)
#
# flujo.entrante.Promethee = function(tab.indices)
# flujo.saliente.Promethee = function(tab.indices)
# flujo.neto.Promethee = function(tab.indices)
#
# flujomed.entrante.Promethee = function(tab.indices)
# flujomed.saliente.Promethee = function(tab.indices)
# flujomed.neto.Promethee = function(tab.indices)



## 6. Método axiomático de Arrow y Raymond

# func_calcula_matrizclasificacion = function(Mvalor)
# multicriterio.metodoaxiomatico.ArrowRaymond = function(Mvalor)





multicriterio.crea.matrizvaloraciones = function(vector_matporfilas,numalternativas=4,
                                                 v.nombres.alternativas=NULL) {
  #X = matrix(c(1,1,1,0,1,0,1,0,0,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1),nrow=5,ncol=5,byrow=TRUE);
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numalternativas,byrow=TRUE);
  #rownames(X) = c('a1','a2','a3','a4','a5');
  #colnames(X) = rownames(X);
  #X
  if (is.null(v.nombres.alternativas)) {
    colnames(X) = paste('a',1:numalternativas,sep='');
    rownames(X) = colnames(X)
  } else{
    colnames(X) = v.nombres.alternativas
    rownames(X) = colnames(X)
  }
  return(X)

}

## ejemplos de uso:
##(Xmat.criterios = multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1),2,c("Rendimiento","Riesgo")))
##(Xmat.criterios = multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1),2))

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

# v_val = c(5, 3, 7, 3,
#           3, 6, 9,
#           1, 3,
#           1)
# m1 = multicriterio.crea.matrizvaloraciones_mej(v_val,numalternativas = 5)
# v_val = c(5, 3, 7, 3,
#           3, 6, 9,
#           1, 3,
#           1)
# vnb = c("Precio","Cercania","Valoraciones","Dormitorios","Utensilios de cocina")
# m1b = multicriterio.crea.matrizvaloraciones_mej(v_val,numalternativas = 5,v.nombres.alternativas = vnb)

multicriterio.crea.matrizdecision = function(vector_matporfilas,numalternativas=3,
                                    numcriterios=4,v.nombresalt=NULL,v.nombrescri=NULL) {

  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numcriterios,byrow=TRUE)
  ##X = matrix(c(100,15,7,40,50,200,25,7,60,200,100,20,4,25,25,200,30,20,70,350,250,25,15,100,500),nrow=5,ncol=5,byrow=TRUE);
  ##rownames(X) = c('a1','a2','a3','a4','a5');
  ##colnames(X) = c('C1','C2','C3','C4','C5');
  ##X
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

## Método 1: maximales (no borrosos) -----


multicriterio.constfuncutilidad.maximales = function(Xmatrizvaloraciones) {
  #browser()
  A1 = Xmatrizvaloraciones
  futil = rep(NA,ncol(A1))
  #names(futil) = paste0("a",1:ncol(A1))
  names(futil) = colnames(A1)
  A1p1 = A1
  diag(A1p1) = rep(0,ncol(A1))
  while (!is.null(dim(A1p1))) {

    #colSums(A1p1)

    alts_min = which(colSums(A1p1)==min(colSums(A1p1)))
    #alts_min

    if (length(alts_min)>1) {

      A2 = A1p1[alts_min,alts_min]
      # investiga cuales están dominados
      Dom = rep(FALSE,length(alts_min))
      for (i in 1:length(alts_min)) {
        #i = 1
        cand = which(A2[,i]==1)
        for (j in cand) {
          if (A2[i,j]==1) {
            # indiferente
          } else {
            Dom[i] = TRUE
          }
        }
      }
      #Dom
      alts_min = alts_min[!Dom]
    } else {
      # alts_min
    }

    for (k in 1:length(alts_min)) futil[names(alts_min)[k]] = ncol(A1p1)
    # futil[names(alts_min)] = ncol(A1p1)
    #futil
    if (length(alts_min)<ncol(A1p1)) {
      A1p1 = A1p1[-alts_min,-alts_min]
    } else {
      A1p1 = NULL
    }
    #A1p1

  }

  futil[is.na(futil)] = 1
  return(futil)
}


## EJEMPLO USO:
## vector = c(1,1,1,0,1,0,1,0,0,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1)
## Xmat = multicriterio.crea.matrizvaloraciones(vector,5)
## funcutilidad.maximal = multicriterio.constfuncutilidad.maximales(Xmat)
## sort(funcutilidad.maximal,decreasing = T)

## Método 2: con estructura borrosa ------

multicriterio.constfuncutilidad.estructuraborrosa = function(Xmatrizvaloraciones) {
  #X = matrix(c(0,0.6,0.2,0.8,0.4,0,0.2,0,0.5,0.1,0,0.1,0.2,0,0.1,0),nrow=4,ncol=4,byrow=TRUE);
  #rownames(X) = c('a1','a2','a3','a4');
  #colnames(X) = rownames(X);
  X = Xmatrizvaloraciones
  flujo.saliente = rowSums(X);
  flujo.saliente
  flujo.entrante = colSums(X);
  flujo.entrante
  flujo.neto = flujo.saliente-flujo.entrante;
  #flujo.neto
  return(flujo.neto)
  #sort(flujo.neto)
}


## EJEMPLO USO:
## vector = c(0,0.6,0.2,0.8,0.4,0,0.2,0,0.5,0.1,0,0.1,0.2,0,0.1,0)
## Xmat = multicriterio.crea.matrizvaloraciones(vector,4)
## funcutilidad.esborrosa = multicriterio.constfuncutilidad.estructuraborrosa(Xmat)
## sort(funcutilidad.esborrosa,decreasing = T)



## Homogeneización tabla decisión: Método 1 Nadir -----

multicriterio.homogeneizacion.nadir = function(Xmatrizdecision) {
  ##X = matrix(c(100,15,7,40,50,200,25,7,60,200,100,20,4,25,25,200,30,20,70,350,250,25,15,100,500),nrow=5,ncol=5,byrow=TRUE);
  ##rownames(X) = c('a1','a2','a3','a4','a5');
  ##colnames(X) = c('C1','C2','C3','C4','C5');
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

## EJEMPLO USO:
## vector = c(100,15,7,40,50,200,25,7,60,200,100,20,4,25,25,200,30,20,70,350,250,25,15,100,500)
## Xmat = multicriterio.crea.matrizdecision(vector,5,5)
## matriz_homogeneizada = multicriterio.homogeneizacion.nadir(Xmat)
## matriz_homogeneizada


## Homogeneización tabla decisión: Método 2 Promethee -----

## función complementaria
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

func_calculo.flujo.neto = function(X.matriz) {
  flujo.saliente = rowSums(X.matriz);
  flujo.entrante = colSums(X.matriz);
  flujo.neto = flujo.saliente-flujo.entrante;
  return(flujo.neto)
}

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


## EJEMPLO USO:
## vector = c(100,15,7,40,50,200,25,7,60,200,100,20,4,25,25,200,30,20,70,350,250,25,15,100,500)
## Xmat = multicriterio.crea.matrizdecision(vector,5,5)
## matriz_homogeneizada = multicriterio.homogeneizacion.promethee(Xmat,c(30,3,4,20,100),c(120,12,10,60,400))
## matriz_homogeneizada


## 3. Método AHP (función de utilidad a partir de comparaciones por pares) -----


### Chequeo matriz comparaciones 2 a 2 correcta

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


# Uso:
#   (Xmat.cri1 = multicriterio.crea.matrizvaloraciones(c(1,1/4,4,4,1,9,1/4,1/9,1),3,nombre.alternativas))
#   multicriterio.metodoAHP.chequeo.matrizcomparaciones(Xmat.cri1)
#
# (Xmat.cri1 = multicriterio.crea.matrizvaloraciones(c(1,1/4,4,1/4,1,9,1/4,1/9,1),3,nombre.alternativas))
# multicriterio.metodoAHP.chequeo.matrizcomparaciones(Xmat.cri1)




### Variante 1: Método del autovector asociado al mayor autovalor.


multicriterio.metodoAHP.variante1.autovectormayorautovalor = function(Xmatrizvaloraciones) {
##C.valora = matrix(c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1),4,4,byrow=T)
##rownames(C.valora) = c('a1','a2','a3','a4');
##colnames(C.valora) = c('a1','a2','a3','a4');

C.valora = Xmatrizvaloraciones

autovalores.C = eigen(C.valora,symmetric=F)
autova.C = Re(autovalores.C$values[Im(autovalores.C$values)==0])
autovec.C = Re(autovalores.C$vectors[,Im(autovalores.C$values)==0])
#autova.C
icual = which.max(autova.C)
#icual
lambda = autova.C[icual]
#lambda
if (is.null(dim(autovec.C))) {
 autovector.v = autovec.C
} else {
 autovector.v = autovec.C[,icual]
}
#autovector.v
#sum(autovector.v)
#sqrt(sum(autovector.v^2))
(valoraciones.finales.ahp = autovector.v/sum(autovector.v))
names(valoraciones.finales.ahp) = colnames(C.valora)
prioridades.relativas = valoraciones.finales.ahp
tablaresumen = C.valora
tablaresumen = cbind(tablaresumen,c(lambda,rep(NA,nrow(C.valora)-1)),autovector.v,prioridades.relativas)
tablaresumen = rbind(tablaresumen,c(rep(NA,ncol(C.valora)+1),sum(autovector.v),NA))

(mm = dim(C.valora)[1]);
(CI.coef.inconsistencia = (lambda-mm)/(mm-1));
##http://www.ccee.edu.uy/ensenian/catmetad/material/MdA-Scoring-AHP.pdf
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

## EJEMPLO USO:
## vector = c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
## Xmat = multicriterio.crea.matrizvaloraciones(vector,4)
## funcutilidad.ahp.var1 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(Xmat)
## sort(funcutilidad.ahp.var1$valoraciones.ahp,decreasing = T)





### Variante 2: Método de los mínimos cuadrados de logaritmos o media geométrica.

func_media.geometrica = function(vector) {
  return( prod(vector)^(1/length(vector)) )
}



multicriterio.metodoAHP.variante2.mediageometrica = function(Xmatrizvaloraciones) {

  C.valora = Xmatrizvaloraciones

  #C.valora = matrix(c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1),4,4,byrow=T)
  #rownames(C.valora) = c('a1','a2','a3','a4');
  #colnames(C.valora) = c('a1','a2','a3','a4');
  #C.valora
  (mgeoms = apply(C.valora,1,func_media.geometrica))
  #sum(mgeoms)
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


## EJEMPLO USO:
## vector = c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1)
## Xmat = multicriterio.crea.matrizvaloraciones(vector,4)
## funcutilidad.ahp.var2 = multicriterio.metodoAHP.variante2.mediageometrica(Xmat)
## funcutilidad.ahp.var2






### Variante 3:  Método de sintetización básico.

##1. **Paso 1**. Sume los valores de cada columna de la matriz de comparación por pares.
##2. **Paso 2**. Divida cada uno de los elementos de la matriz de comparación por pares entre el total de su columna; la matriz resultante se conoce como **matriz de comparación por pares normalizada**.
##3. **Paso 3**. Calcule la media de los elementos de cada fila de la matriz normalizada; estas medias nos dan una estimación de las prioridades relativas de los elementos que se están comparando.


multicriterio.metodoAHP.variante3.basico = function(Xmatrizvaloraciones) {

  #C.valora = matrix(c(1,2,6,9,1/2,1,4,9,1/6,1/4,1,3,1/9,1/9,1/3,1),4,4,byrow=T)
  #rownames(C.valora) = c('a1','a2','a3','a4');
  #colnames(C.valora) = c('a1','a2','a3','a4');
  C.valora = Xmatrizvaloraciones

  (suma.columnas = colSums(C.valora));
  (m.suma.columnas = matrix(suma.columnas,length(suma.columnas),
                            length(suma.columnas),byrow=T))
  (C.valora.normalizada = C.valora/m.suma.columnas)
  (prioridades.relativas = rowMeans(C.valora.normalizada))

  tablaresumen = C.valora
  tablaresumen = cbind(tablaresumen,C.valora.normalizada,prioridades.relativas)
  tablaresumen = rbind(tablaresumen,c(suma.columnas,rep(NA,ncol(C.valora)+1)))
  #### Usando el Método de sintetización básico
  ## Funciones (Francisco José Solís)
  ##criterio.simple <- function(matriz)
  ##  return(rowMeans(matriz/matrix(c(colSums(matriz)),nrow=dim(matriz)[1],ncol=dim(matriz)[2],byrow=TRUE)))

  salida = list()
  salida$Xmat = C.valora
  salida$sumacolumnas = suma.columnas
  salida$Xmat.normalizada = C.valora.normalizada
  salida$valoraciones.ahp = prioridades.relativas
  salida$valoraciones.ahp.ordenadas = sort(prioridades.relativas,decreasing = T)
  salida$tablaresumen = tablaresumen
  return(salida)

}


### Método AHP: obtención de los pesos globales de las alternativas

##Las siguientes funciones nos permiten obtener los pesos globales de las alternativas según el método AHP,
##a partir de las matrices de comparación 2 a 2:

##ahp.pesos.finales <- function(pesos.criterios,pesos.locales)
multicriterio.metodoAHP.pesosglobales = function(pesos.criterios,matriz.pesos.locales) {
  vector1 = as.numeric(pesos.criterios%*%matriz.pesos.locales)
  names(vector1) = colnames(matriz.pesos.locales)
  return(vector1)
}


##ahp.pesos.finales <- function(pesos.criterios,pesos.locales)
multicriterio.metodoAHP.pesosglobales_entabla = function(pesos.criterios,
                                                         matriz.pesos.locales) {
  vector1 = as.numeric(pesos.criterios%*%matriz.pesos.locales)
  #names(vector1) = colnames(matriz.pesos.locales)

  tablafinal = rbind(t(matriz.pesos.locales),pesos.criterios)
  tablafinal = cbind(tablafinal,c(vector1,NA))
  rownames(tablafinal)[nrow(tablafinal)] = "Ponder.Criterios"
  colnames(tablafinal)[ncol(tablafinal)] = "Ponderadores Globales"

  return(tablafinal)
}



##La siguiente función nos permite calcular los coeficientes de inconsistencia
##  para cada matriz de comparación 2 a 2:

multicriterio.metodoAHP.coef.inconsistencia = function(Xmatrizvaloraciones) {
  C.valora = Xmatrizvaloraciones
  autovalores.C = eigen(C.valora,symmetric=F);
  autova.C = Re(autovalores.C$values[Im(autovalores.C$values)==0]);
  icual = which.max(autova.C);
  lambda = autova.C[icual];
  mm = dim(C.valora)[1];
  CI.coef.inconsistencia = (lambda-mm)/(mm-1);
  #CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24);
  CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49);
  RI.coef.inconsistencia = CI.coef.inconsistencia/CA.coefs.inconsistencia.aleatorio[mm];
  ##RI.coef.inconsistencia
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



##multicriterio.metodoAHP.variante3.basico.coef.inconsistencia(C.valora)
multicriterio.metodoAHP.variante3.basico.coef.inconsistencia = function(Xmatrizvaloraciones) {


  #C.valora = matrix(c(1,3,6,1/3,1,3,1/6,1/3,1),3,3,byrow=T)
  #rownames(C.valora) = c('a1','a2','a3');
  #colnames(C.valora) = c('a1','a2','a3');
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
  #CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24);
  CA.coefs.inconsistencia.aleatorio = c(NA,0.00,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49);
  RI.coef.inconsistencia = CI.coef.inconsistencia/CA.coefs.inconsistencia.aleatorio[mm];
  ##RI.coef.inconsistencia
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


##Xmatriznivel2 = array(NA,dim=c(num.alt,num.alt,num.cri))
##Xmatriznivel2[,,1] =

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



# (Xmat.criterios = multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1),2,c("Rendimiento","Riesgo")))
# (Xmat.rendimiento = multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1),2,c("Alt. A","Alt. B")))
# (Xmat.riesgo = multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1),2,c("Alt. A","Alt. B")))
#
# num.alt = 2
# num.cri = 2
# Xmatriznivel2 = array(NA,dim=c(num.alt,num.alt,num.cri))
# Xmatriznivel2[,,1] = Xmat.rendimiento
# Xmatriznivel2[,,2] = Xmat.riesgo
# multicriterio.metodoAHP.variante3.completo(Xmat.criterios,Xmatriznivel2)



# # Ejercicio 4
#
# (Xmat.criterios = multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1),2,c("Rendimiento","Riesgo")))
# (Xmat.rendimiento = multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1),2,c("Alt. A","Alt. B")))
# (Xmat.riesgo = multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1),2,c("Alt. A","Alt. B")))
#
# (pesos.criterios = multicriterio.metodoAHP.variante3.basico(Xmat.criterios))
# (pesos.rendimiento = multicriterio.metodoAHP.variante3.basico(Xmat.rendimiento))
# (pesos.riesgo = multicriterio.metodoAHP.variante3.basico(Xmat.riesgo))
#
#
# multicriterio.metodoAHP.coef.inconsistencia(Xmat.criterios)
# multicriterio.metodoAHP.coef.inconsistencia(Xmat.rendimiento)
# multicriterio.metodoAHP.coef.inconsistencia(Xmat.riesgo)
# (pesosglobales.ahp = multicriterio.metodoAHP.pesosglobales(pesos.criterios$valoraciones.ahp,rbind(pesos.rendimiento$valoraciones.ahp,pesos.riesgo$valoraciones.ahp)))
# sort(pesosglobales.ahp,T)


### Mejoras Tabla AHP (formattable) ------


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
#export_formattable(AnalyzeTable(carAhp), file = "table01.png")
#export_formattable(lres$tb01sal, file = "table01.png")


#library(formattable)
multicriterio.metodoAHP.pesosglobales_formattable = function(tb.nivel01, l_tb.nivel02,
                                                             que.variante = 1) {


    if (que.variante == 1) {
        #Calculamos los pesos locales:
        pesos.nivel01 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tb.nivel01)

        l_pesos.nivel02 = lapply(1:length(l_tb.nivel02),
                                 function(x)
                                     multicriterio.metodoAHP.variante1.autovectormayorautovalor(l_tb.nivel02[[x]])
        )

    } else if (que.variante == 2) {
        #Calculamos los pesos locales:
        pesos.nivel01 = multicriterio.metodoAHP.variante2.mediageometrica(tb.nivel01)

        l_pesos.nivel02 = lapply(1:length(l_tb.nivel02),
                                 function(x)
                                     multicriterio.metodoAHP.variante2.mediageometrica(l_tb.nivel02[[x]])
        )

    } else if (que.variante == 3) {
        #Calculamos los pesos locales:
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
    #browser()
    m_pesos.nivel02 = do.call(rbind,
                              lapply(1:length(l_pesos.nivel02), function(x) l_pesos.nivel02[[x]]$valoraciones.ahp)
    )
    # Calculamos ahora los pesos globales:

    pglobales = multicriterio.metodoAHP.pesosglobales_entabla(pesos.nivel01$valoraciones.ahp,
                                                              m_pesos.nivel02)

    ind_ord_r = c(nrow(pglobales), 1:(nrow(pglobales)-1) )
    ind_ord_c = c(ncol(pglobales), 1:(ncol(pglobales)-1) )
    pglobales_mej = t(pglobales[ind_ord_r,ind_ord_c])
    pglobales_mej[1,1] = 1

    pglobales_mej2 = pglobales_mej
    pglobales_mej2[1,-1] = NA

    # BUENO: t2

    formatea_NA = formatter("span",
                            style = x ~ style(color = ifelse(is.na(x), "white", x)))
    formatea_negrita <- formatter("span",
                                  style = x ~ style("font-weight" = ifelse(is.na(x), "bold", NA)))
    # formatea_inconsistencia <- formatter("span",
    #                               x ~ icontext(ifelse(x>=100,"remove",NA),x),
    #                               style = x ~ style(background = ifelse(x>=100, "yellow","lightyellow")))
    formatea_inconsistencia <- formatter("span",
                                         x ~ icontext(ifelse(x>=0.10,"remove","ok"),paste0(round(100*x,2),"%")),
                                         style = x ~ style(display = "block",
                                                           padding = "0 4px", #padding = "0 4px 0 -40px",
                                                           `border-radius` = "4px",
                                                           color = ifelse(x>=0.10,"red","green"),
                                                           background = ifelse(x>=0.10, "yellow",csscolor(gradient(as.numeric(x),"lightyellow","#ffff70"))))
    )

    # https://help.displayr.com/hc/en-us/articles/360003132036-How-to-Customize-a-Table-Using-the-Formattable-R-Package
    # sin inconsistencias
    # dtpg04mej3 = as.data.frame(pglobales_mej2)
    # dtpg04mej3 = cbind("Pesos Locales AHP" = rownames(dtpg04mej3),
    #                    dtpg04mej3)
    # rownames(dtpg04mej3) =  c()
    # dtpg04mej3[1,1] = "Peso Total"
    # colnames(dtpg04mej3)[2] = "Criterios/Alternativas"
    # #str(dtpg04mej3)
    #
    # tb02sal = (dtpg04mej3) |>
    #     formattable(
    #         align = c("l",rep("r",(ncol(dtpg04mej3)-1))),
    #         list(
    #             area(col = -1) ~ percent,
    #             area(row = 1,col = -1) ~ formatea_NA,
    #             area(col = 2, row = -1) ~ color_tile("#ecf0ec","#B4C3B4"),
    #             area(row = 2:nrow(dtpg04mej3), col = -(1:2)) ~ color_tile("#ddd9dd","#786778"),
    #             #area(row = 2:nrow(dtpg04mej3), col = 3) ~ color_tile("#ddd9dd","#786778"),
    #             #area(row = 2:nrow(dtpg04mej3), col = 4) ~ color_tile("#ddd9dd","#786778"),
    #             area(col = 1) ~ formatter("span",
    #                                       style = x ~ style("font-weight" = "bold"))
    #         )
    #     )
    #csscolor(gradient(c(1,2,3,4,5), "transparent", "lightblue"))
    #csscolor(gradient(c(1,2,3,4,5), "transparent", "lightyellow"))
    #csscolor(gradient(c(1,2,3,4,5), "lightyellow","yellow"))

    # CON INCONSISTENCIAS: REVISANDO
    dtpg04mej3 = as.data.frame(pglobales_mej2)
    dtpg04mej3 = cbind("Pesos Locales AHP" = rownames(dtpg04mej3),
                       dtpg04mej3)
    rownames(dtpg04mej3) =  c()
    dtpg04mej3[1,1] = "Peso Total"
    colnames(dtpg04mej3)[2] = "Criterios/Alternativas"
    dtpg04mej3$Inconsistencia = v_incons_todos
    #str(dtpg04mej3)
    ## browser()
    tb02sal = (dtpg04mej3) |>
        formattable(
            align = c("l",rep("r",(ncol(dtpg04mej3)-1))),
            list(
                area(col = -1) ~ percent,
                area(row = 1,col = -1) ~ formatea_NA,
                area(col = 2, row = -1) ~ color_tile("#ecf0ec","#B4C3B4"),
                area(row = 2:nrow(dtpg04mej3), col = -c(1:2,ncol(dtpg04mej3))) ~ color_tile("#ddd9dd","#786778"),
                #area(row = 2:nrow(dtpg04mej3), col = 3) ~ color_tile("#ddd9dd","#786778"),
                #area(row = 2:nrow(dtpg04mej3), col = 4) ~ color_tile("#ddd9dd","#786778"),
                area(col = 1) ~ formatter("span",
                                          style = x ~ style("font-weight" = "bold")),
                #area(col = ncol(dtpg04mej3)) ~ percent,
                area(col = ncol(dtpg04mej3)) ~ formatea_inconsistencia
                #color_tile("#fffff7","lightyellow")

            )
        )


    # BUENO?: t1

    dtpg04mej10 = as.data.frame(pglobales_mej2)

    dtpg04mej11 = dtpg04mej10[,1] * dtpg04mej10[,-1]
    #dtpg04mej11v = colSums(dtpg04mej11[-1,])
    dtpg04mej11v = colSums(dtpg04mej11, na.rm = T)
    dtpg04mej11[1,] = dtpg04mej11v

    dtpg04mej10[,-1] = dtpg04mej11
    dtpg04mej12 = dtpg04mej10

    dtpg04mej12b = cbind("Interpretacion-Aportaciones AHP" = rownames(dtpg04mej12),
                         dtpg04mej12)
    rownames(dtpg04mej12b) =  c()
    colnames(dtpg04mej12b)[2] = "Ponderaciones Criterios"
    #str(dtpg04mej12b)

    tb01sal = (dtpg04mej12b) |>
        formattable(
            align = c("l",rep("r",(ncol(dtpg04mej12b)-1))),
            list(
                area(col = -1) ~ percent,
                area(col = 2, row = -1) ~ color_tile("#ecf0ec","#B4C3B4"),
                #lapply(3:ncol(dtpg04mej12b), function(x) {
                #area(row = -1, col = x) ~ color_tile("#ddd9dd","#786778")
                #}),
                area(col = -(1:2)) ~ color_tile("#ddd9dd","#786778"),
                #area(row = -1, col = 3) ~ color_tile("#ddd9dd","#786778"),
                #area(row = -1, col = 4) ~ color_tile("#ddd9dd","#786778"),
                #area(row = 1, col = -(1:2)) ~ color_tile("#d6ebf2", "lightblue"),
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



### Multicriterio Electre ------


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
  #browser()
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
      if ((suma2==0) | ((suma1!=0) & (suma1 == suma2))) {  # suma1==0 equivale a Imenos es vacío
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

  ##tabdecs.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan,que.alternativas=TRUE
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


# tabdec.X = matrix(c(20, 13, 15, 30, 5, 40,0.3, 0.5, 0.1, 0.7, 0.9, 0,1.3, 4, 2.2, 1, 4, 1,3, 3, 5, 2, 7, 1),4,6,byrow=T);
# colnames(tabdec.X) = c('a1', 'a2', 'a3', 'a4', 'a5', 'a6');
# rownames(tabdec.X) = c('V1', 'V2', 'V3', 'V4');
# tabdec.X = t(tabdec.X)
# tabdec.X
#
# pesos.criterios = c(0.3,0.2,0.4,0.1);
# nivel.concordancia.minimo.alpha = 0.7;
# no.se.compensan = c(15,Inf,2,Inf);
#
#
# paso01 = multicriterio.metodoELECTRE_I(tabdec.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan);
# paso01;
#
# paso02 = multicriterio.metodoELECTRE_I(tabdec.X,pesos.criterios,nivel.concordancia.minimo.alpha,no.se.compensan,c(1,2));
# paso02;


### Método Electre I variante del libro de Miguel Córdoba Bueno: "Metodología para la toma de decisiones"

# Mvalor = matrix(c(8,25,20,10,75,5,
#  12,35,17,25,100,7,
#  10,40,40,35,125,2,
#  15,40,20,20,35,4,
#  12,35,15,45,75,6),nrow=5,ncol=6,byrow=T)
#  rownames(Mvalor) = c("A1","A2","A3","A4","A5")
#  colnames(Mvalor) = c("C1","C2","C3","C4","C5","C6")
#
# Mvalor[,5] = -Mvalor[,5]
# Mvalor[,6] = -Mvalor[,6]
# # SON TODAS DE MAXIMIZACIÓN
# Mvalor
#
# pesos.criterios = c(0.15,0.25,0.20,0.10,0.20,0.10)
# multicriterio.metodoelectre_varlibro(Mvalor, pesos.criterios,umbral.c=NA,umbral.d=NA)
# multicriterio.metodoelectre_varlibro(Mvalor, pesos.criterios)
# multicriterio.metodoelectre_varlibro(Mvalor, pesos.criterios,umbral.c=0.66,umbral.d=0.7)
# multicriterio.metodoelectre_varlibro(Mvalor, pesos.criterios,umbral.c=0.7,umbral.d=0.81)


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
  #Mindconcor

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
  Minddiscor

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





###
## 5. Método Promethee I y II ------
###

## funciones para Promethee

fpref.criterio_usual = function(vaj,vah) {
  di = vaj-vah;
  if (di <= 0) {
    res = 0;
  } else {
    res = 1;
  }
  return(res);
}

fpref.cuasi_criterio = function(vaj,vah,qi) {
  di = vaj - vah;
  if (di <= qi) {
    res = 0;
  } else {
    res = 1;
  }
  return(res);
}

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

fpref.criterio_gaussiano = function(vaj,vah,qi,pi,si) {
  di = vaj - vah;
  if (di <= 0) {
    res = 0;
  } else {
    res = 1 - exp((-di^2)/(2*si^2));
  }
  return(res);
}

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


###


##Definimos la función **índice de preferencia multicriterio** para cada par de alternativas:

indice.prefmulticriterio = function(tabdecs.X,pesos.criterios,tab.fpref) {
  ##tabdecs.X = tabdec.X
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


##Cálculo de **flujos entrantes**, **flujos salientes**:

flujo.entrante.Promethee = function(tab.indices) {
  res = rowSums(tab.indices);
  return(res);
}

flujo.saliente.Promethee = function(tab.indices) {
  res = colSums(tab.indices);
  return(res);
}

flujo.neto.Promethee = function(tab.indices) {
  vflujo.ent = flujo.entrante.Promethee(tab.indices);
  vflujo.sal = flujo.saliente.Promethee(tab.indices);
  res = vflujo.ent - vflujo.sal;
  return(res);
}


## En algunas ocasiones, se utilizan como **flujos entrantes** y **flujos salientes**
## las medias por fila y columnas (pero sin considerar el elemento diagonal que es 0
## siempre), y no la suma:

flujomed.entrante.Promethee = function(tab.indices) {
  res = rowSums(tab.indices)/(dim(tab.indices)[1]-1);
  return(res);
}

flujomed.saliente.Promethee = function(tab.indices) {
  res = colSums(tab.indices)/(dim(tab.indices)[1]-1);
  return(res);
}

flujomed.neto.Promethee = function(tab.indices) {
  vflujo.ent = flujomed.entrante.Promethee(tab.indices);
  vflujo.sal = flujomed.saliente.Promethee(tab.indices);
  res = vflujo.ent - vflujo.sal;
  return(res);
}



## Clasificación del **método Promethee I**:

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
  ##return(tab.Pthee.i);
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.ent=vflujo.ent
  salida$vflujos.sal=vflujo.sal
  salida$tablarelacionsupera = tab.Pthee.i
  return(salida)

}



## Clasificación del **método Promethee II**:

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
  #return(tab.Pthee.ii);
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.netos=vflujos.netos
  salida$tablarelacionsupera = tab.Pthee.ii
  return(salida)

}



## Las siguientes funciones, son una variante de los anteriores,
## en la que se utilizan los flujos medios:

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
  ##return(tab.Pthee.i);
  salida = list()
  salida$tabla.indices = tab.indices
  salida$vflujos.ent=vflujo.ent
  salida$vflujos.sal=vflujo.sal
  salida$tablarelacionsupera = tab.Pthee.i
  return(salida)
}



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
  ##return(tab.Pthee.ii);
  return(salida)
}



###
### Método axiomático de Arrow y Raymond -----
###


# Mvalor = matrix(c(8,25,20,10,75,5,
# 12,35,17,25,100,7,
# 10,40,40,35,125,2,
# 15,40,20,20,35,4,
# 12,35,15,45,75,6),nrow=5,ncol=6,byrow=T)
# rownames(Mvalor) = c("A1","A2","A3","A4","A5")

func_calcula_matrizclasificacion = function(Mvalor) {
  #Mvalor
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

multicriterio.metodoaxiomatico.ArrowRaymond = function(Mvalor) {

  num.alter = nrow(Mvalor)
  salida = vector("list",num.alter-1)
  salidatemp = list()
  Mvalor_red = Mvalor
  alternativas.salen = c()
  #i=1
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


#final = multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor)
#final
