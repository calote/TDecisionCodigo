

# Funciones auxiliares para metodos multicriterio ----


#' Generar criterio aleatorio
#'
#' Genera un vector de valores aleatorios enteros que representa un criterio
#' de evaluacion.
#'
#' @param nalt Numerico. Numero de alternativas (valores a generar).
#' @param Minimo Numerico. Valor minimo del rango (por defecto 0).
#' @param Maximo Numerico. Valor maximo del rango (por defecto 1000).
#'
#' @return Vector numerico entero con los valores del criterio.
#'
#' @examples
#' set.seed(123)
#' func_genera_Criterio(5, 0, 100)
#'
#' @export
func_genera_Criterio = function(nalt,Minimo=0,Maximo=1000) {
  VCri = floor(runif(nalt,Minimo,Maximo))
  return(VCri)
}


#' Convertir criterio a matriz de valoraciones 0/1
#'
#' Convierte un vector de valores de criterio en una matriz cuadrada de
#' valoraciones binarias (0/1).
#'
#' @param vc_Val Vector numerico con los valores del criterio.
#' @param Desig Logico. Si TRUE, usa >= (desigualdad debil). Si FALSE, usa >.
#'
#' @return Matriz cuadrada donde el elemento (i,j) vale 1 si la alternativa
#'   i es al menos tan buena como la j segun el criterio.
#'
#' @examples
#' func_conv_a_01_Criterio(c(50, 30, 80))
#' func_conv_a_01_Criterio(c(50, 30, 80), Desig = FALSE)
#'
#' @export
func_conv_a_01_Criterio = function(vc_Val,Desig=TRUE) {
  mSal = matrix(NA,length(vc_Val),length(vc_Val))
  for (i1 in 1:length(vc_Val)) {
    for (i2 in 1:length(vc_Val)) {
      if (Desig) {
        if (vc_Val[i1]>=vc_Val[i2]) {
          mSal[i1,i2] = 1
        } else {
          mSal[i1,i2] = 0
        }
      } else {
        if (vc_Val[i1]>vc_Val[i2]) {
          mSal[i1,i2] = 1
        } else {
          mSal[i1,i2] = 0
        }
      }
    }
  }
  return(mSal)
}


#' Generar matriz de valoraciones aleatoria
#'
#' Genera una matriz cuadrada de valoraciones a partir de un criterio aleatorio.
#'
#' @param nalt Numerico. Numero de alternativas.
#' @param Minimo Numerico. Valor minimo del criterio.
#' @param Maximo Numerico. Valor maximo del criterio.
#' @param Desig Logico. Si TRUE, usa >=. Si FALSE, usa >.
#'
#' @return Matriz cuadrada de valoraciones binarias.
#'
#' @examples
#' set.seed(123)
#' func_genera_Mvaloraciones(5)
#'
#' @export
func_genera_Mvaloraciones = function(nalt,Minimo=0,Maximo=1000,Desig=TRUE) {
  VCri = func_genera_Criterio(nalt,Minimo,Maximo)
  A1 = func_conv_a_01_Criterio(VCri,Desig)
  colnames(A1) = paste0("a",1:ncol(A1))
  rownames(A1) = colnames(A1)
  return(A1)
}


#' Generar matriz de decision aleatoria
#'
#' Genera una matriz de decision donde las filas son alternativas y las
#' columnas son criterios, con valores aleatorios enteros.
#'
#' @param nalt Numerico. Numero de alternativas (filas).
#' @param ncri Numerico. Numero de criterios (columnas).
#' @param vMin Vector numerico con los valores minimos por criterio.
#' @param vMax Vector numerico con los valores maximos por criterio.
#'
#' @return Matriz numerica de decision.
#'
#' @examples
#' func_genera_MatrizCriterios(5, 3)
#' func_genera_MatrizCriterios(4, 2, c(10, 20), c(100, 200))
#'
#' @export
func_genera_MatrizCriterios = function(nalt,ncri,vMin=rep(0,ncri),vMax=rep(1000,ncri)) {
  VCri = matrix(rep(NA,ncri*nalt),nalt,ncri)
  for (i in 1:ncri) {
    VCri[,i] = floor(runif(nalt,vMin[i],vMax[i]))
  }
  colnames(VCri) = paste0("C",1:ncol(VCri))
  rownames(VCri) = paste0("a",1:nrow(VCri))
  return(VCri)
}


#' Convertir matriz de criterios a beneficios
#'
#' Convierte las columnas de coste de una matriz de decision a beneficios
#' invirtiendo el signo.
#'
#' @param MCri Matriz numerica de decision (alternativas x criterios).
#' @param vl_Ben Vector logico. TRUE indica beneficio, FALSE indica coste.
#'
#' @return Matriz numerica donde todas las columnas representan beneficios.
#'
#' @examples
#' MCri <- matrix(c(10, 20, 5, 8), nrow = 2, ncol = 2)
#' func_cambia_MatrizCriterios_SonBeneficios(MCri, c(TRUE, FALSE))
#'
#' @export
func_cambia_MatrizCriterios_SonBeneficios = function(MCri,vl_Ben=rep(TRUE,ncol(MCri))) {
    vsigno = ifelse(vl_Ben,1,-1)
    msigno = matrix(vsigno,nrow(MCri),ncol(MCri),byrow=TRUE)
    MCri_sal = MCri * msigno
    return(MCri_sal)
}


#' Comprobar transitividad de una relacion
#'
#' Verifica si la relacion de preferencia representada en una matriz es
#' transitiva para una terna de alternativas (i, j, k).
#'
#' @param A Matriz cuadrada de relacion de preferencia.
#' @param i Numerico. Indice de la primera alternativa.
#' @param j Numerico. Indice de la segunda alternativa.
#' @param k Numerico. Indice de la tercera alternativa.
#'
#' @return Logico. TRUE si se cumple la transitividad, FALSE en caso contrario.
#'
#' @examples
#' A <- matrix(c(1,1,0,0,1,1,1,0,1), nrow=3, byrow=TRUE)
#' func_comprueba_transitiva(A, 1, 2, 3)
#'
#' @export
func_comprueba_transitiva = function(A,i,j,k) {
  res = TRUE
  if (A[i,j]==1) {
    if (A[j,k]==1) {
      if ((A[k,i]==1) & (A[i,k]==0) ){
        res = FALSE
      }
    }
  }
  return(res)
}


#' Comprobar transitividad de una matriz de valoraciones
#'
#' Verifica si una matriz cuadrada de valoraciones cumple la propiedad
#' de transitividad para todas las ternas de alternativas.
#'
#' @param A1 Matriz cuadrada de valoraciones.
#'
#' @return Logico. TRUE si toda la matriz es transitiva, FALSE si alguna
#'   terna no cumple.
#'
#' @examples
#' A <- matrix(c(1,1,1,0,1,1,0,0,1), nrow=3, byrow=TRUE)
#' func_comprueba_transitiva_MValoraciones(A)
#'
#' @export
func_comprueba_transitiva_MValoraciones = function(A1) {

  for (i in 1:ncol(A1)) {
    for (j in 1:ncol(A1)) {
      if (j!=i) {
        for (k in 1:ncol(A1)) {
          if ((j!=k) & (k!=i)) {
            if (!func_comprueba_transitiva(A1,i,j,k)) {
              message(i,j,k,": no cumplen transitiva")
              return(FALSE)
            }
          }
        }
      }
    }
  }
  return(TRUE)
}


## Funciones para ELECTRE ----

#' @noRd
func_ELECTRE_malts_T = function(array1) {
  dims = dim(array1)
  msal = matrix("",dims[1],dims[2])
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      for (k in 1:dims[3]) {
        if (array1[i,j,k]) {
          if (msal[i,j]=="") {
            msal[i,j] = as.character(k)
          } else {
            msal[i,j] = paste(msal[i,j],k,sep=",")
          }
        }
      }
    }
  }
  return(msal)
}


#' Resultados ELECTRE: Tabla de indices
#'
#' Genera una tabla formateada con los indices de relacion de ELECTRE
#' (I+, I=, I-).
#'
#' @param res_electre Lista resultado de \code{multicriterio.metodoELECTRE_I}.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{MIndices}{DataFrame con los indices}
#'   \item{KE}{Tabla kableExtra formateada}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' res_electre <- multicriterio.metodoELECTRE_I(tabdec, c(0.3, 0.2, 0.4, 0.1),
#'                                             0.7, c(15, Inf, 2, Inf))
#' resultado <- func_ELECTRE_resMIndices(res_electre)
#' resultado$MIndices
#'
#' @export
func_ELECTRE_resMIndices = function(res_electre) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function. Please install it with install.packages(\"dplyr\").", call. = FALSE)
  }
  mi1 = func_ELECTRE_malts_T(res_electre$Imas)
  mi2 = func_ELECTRE_malts_T(res_electre$Iigual)
  mi3 = func_ELECTRE_malts_T(res_electre$Imenos)

  for (i in 1:nrow(mi1)) {
    mf = rbind(mi1[i,],mi2[i,],mi3[i,])
    if (i==1) {
      msal = mf
    } else {
      msal = rbind(msal,mf)
    }
  }
  msal = as.data.frame(msal)
  colnames(msal) = paste0("A",1:ncol(msal))

  msal$Inds = rep(c("I+","I=","I-"),nrow(msal) %/% 3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]

  msal$Alts = rep(paste0("A",1:(ncol(msal)-1)),each=3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  msal0 = msal

  KE_msal = msal %>%
    knitr::kable(booktabs=TRUE) %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(0,align="c",color="blue")
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>%
      kableExtra::row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=TRUE)
  }
  KE_msal = KE_msal %>%
    kableExtra::column_spec(1,background = "white",width = "4em",bold = TRUE,color="blue") %>%
    kableExtra::column_spec(2,width = "4em",color="red",bold=TRUE,border_right = TRUE) %>%
    kableExtra::column_spec(3:ncol(msal),width="10em") %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::footnote(general = "ELECTRE: Conjunto de Indices",general_title = "")

  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)
}


#' Resultados ELECTRE: Tabla de concordancia
#'
#' Genera una tabla formateada con los indices de concordancia de ELECTRE.
#'
#' @param res_electre Lista resultado de \code{multicriterio.metodoELECTRE_I}.
#' @param digitos Numerico. Numero de decimales para redondear.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{MIndices}{DataFrame con los indices de concordancia}
#'   \item{KE}{Tabla kableExtra formateada}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' res_electre <- multicriterio.metodoELECTRE_I(tabdec, c(0.3, 0.2, 0.4, 0.1),
#'                                             0.7, c(15, Inf, 2, Inf))
#' resultado <- func_ELECTRE_resTConcordancia(res_electre)
#' resultado$MIndices
#'
#' @export
func_ELECTRE_resTConcordancia = function(res_electre,digitos=4) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
  mi1 = res_electre$ind.concordancia
  mi2 = res_electre$ind.concordancia.gorro
  mi3 = res_electre$test.concordancia

  for (i in 1:nrow(mi1)) {
    mf = rbind(mi1[i,],mi2[i,],mi3[i,])
    if (i==1) {
      msal = mf
    } else {
      msal = rbind(msal,mf)
    }
  }
  msal = round(msal,digits = digitos)
  msal = as.data.frame(msal)
  colnames(msal) = paste0("A",1:ncol(msal))

  msal$Inds = rep(c("Ijk","IGjk","TC"),nrow(msal) %/% 3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]

  msal$Alts = rep(paste0("A",1:(ncol(msal)-1)),each=3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  msal0 = msal

  filas = seq(3,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = kableExtra::cell_spec(ifelse(msal[filas,3:ncol(msal)]>=1,"T","F"),
                                   background = ifelse(msal[filas,3:ncol(msal)]>=1,"blue","white"),
                                   color=ifelse(msal[filas,3:ncol(msal)]>=1,"white","black"))

  KE_msal = msal %>%
    kableExtra::kbl(booktabs=TRUE,digits = 3,escape = FALSE,format = "html") %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(0,align="c",color="blue")
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>%
      kableExtra::row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=TRUE)
  }
  KE_msal = KE_msal %>%
    kableExtra::column_spec(1,background = "white",width = "4em",bold = TRUE,color="blue") %>%
    kableExtra::column_spec(2,width = "4em",color="red",bold=TRUE,border_right = TRUE) %>%
    kableExtra::column_spec(3:ncol(msal),width="10em") %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::footnote(general = "ELECTRE: Test Concordancia",general_title = "")

  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)
}


#' Resultados ELECTRE: Tabla de discordancia
#'
#' Genera una tabla formateada con los test de discordancia de ELECTRE.
#'
#' @param res_electre Lista resultado de \code{multicriterio.metodoELECTRE_I}.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{MIndices}{DataFrame con los indices de discordancia}
#'   \item{KE}{Tabla kableExtra formateada}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' res_electre <- multicriterio.metodoELECTRE_I(tabdec, c(0.3, 0.2, 0.4, 0.1),
#'                                             0.7, c(15, Inf, 2, Inf))
#' resultado <- func_ELECTRE_resTDiscordancia(res_electre)
#' resultado$MIndices
#'
#' @export
func_ELECTRE_resTDiscordancia = function(res_electre) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function. Please install it with install.packages(\"stringr\").", call. = FALSE)
  }
  s1 = func_ELECTRE_resMIndices(res_electre)
  filas = seq(3,nrow(s1$MIndices),by=3)
  mf1 = s1$MIndices[filas,3:ncol(s1$MIndices)]
  mf1 = apply(mf1,2,as.character)
  md = res_electre$datos
  vd = md[nrow(md),]
  md = md[1:(nrow(md)-2),]
  mf2 = matrix(NA,nrow(mf1),ncol(mf1))
  mf3 = matrix(TRUE,nrow(mf1),ncol(mf1))
  for (i in 1:nrow(mf1)) {
    for (j in 1:ncol(mf1)) {
       b1 = stringr::str_trim(mf1[i,j])
       b2l = as.integer(unlist(stringr::str_split(b1,",")))
       if (i==j) {
         mf2[i,j] = ""
         mf3[i,j] = TRUE
         next
       }
       if (length(b2l)>1) {
         crit = b2l[1]
         dif0 = md[j,crit] - md[i,crit]
         mf2[i,j] = paste0(dif0,"(",vd[crit],")")
         if (dif0>=vd[crit]) {
           mf3[i,j] = FALSE
         }
         for (k in 2:length(b2l)) {
           crit = b2l[k]
           dif0 = md[j,crit] - md[i,crit]
           if (dif0>=vd[crit]) {
             mf3[i,j] = FALSE
           }
           mf2[i,j] = paste0(mf2[i,j],",",dif0,"(",vd[crit],")")
         }
       } else {
         if (!is.na(b2l)) {
           crit = b2l[1]
           dif0 = md[j,crit] - md[i,crit]
           if (dif0>=vd[crit]) {
             mf3[i,j] = FALSE
           }
           mf2[i,j] = paste0(dif0,"(",vd[crit],")")
         }
       }
    }
  }

  mi1 = mf1
  mi2 = mf2
  mi3 = matrix(as.character(as.numeric(mf3)),nrow(mf3),ncol(mf3))

  for (i in 1:nrow(mi1)) {
    mf = rbind(mi1[i,],mi2[i,],mi3[i,])
    if (i==1) {
      msal = mf
    } else {
      msal = rbind(msal,mf)
    }
  }
  msal = as.data.frame(msal,stringsAsFactors = FALSE)
  colnames(msal) = paste0("A",1:ncol(msal))

  msal$Inds = rep(c("I-","Djk","TD"),nrow(msal) %/% 3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]

  msal$Alts = rep(paste0("A",1:(ncol(msal)-1)),each=3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  msal0 = msal

  filas = seq(3,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = kableExtra::cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","blue","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))

  KE_msal = msal %>%
    kableExtra::kbl(booktabs=TRUE,digits = 3,escape = FALSE) %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(0,align="c",color="blue")
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>%
      kableExtra::row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=TRUE)
  }
  KE_msal = KE_msal %>%
    kableExtra::column_spec(1,background = "white",width = "4em",bold = TRUE,color="blue") %>%
    kableExtra::column_spec(2,width = "4em",color="red",bold=TRUE,border_right = TRUE) %>%
    kableExtra::column_spec(3:ncol(msal),width="10em") %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::footnote(general = paste0("ELECTRE: Test Discordancia: ",
                              "vd=(",paste0(vd,collapse=","),")"
                              ),general_title = "")

  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)

}


#' Resultados ELECTRE: Tabla de superacion
#'
#' Genera una tabla formateada con la relacion de superacion de ELECTRE
#' (concordancia x discordancia).
#'
#' @param res_electre Lista resultado de \code{multicriterio.metodoELECTRE_I}.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{MIndices}{DataFrame con los indices de superacion}
#'   \item{KE}{Tabla kableExtra formateada}
#'   \item{Grafo}{DataFrame con las relaciones de dominancia}
#'   \item{Nucleo}{Indices del nucleo}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' res_electre <- multicriterio.metodoELECTRE_I(tabdec, c(0.3, 0.2, 0.4, 0.1),
#'                                             0.7, c(15, Inf, 2, Inf))
#' resultado <- func_ELECTRE_resTSuperacion(res_electre)
#' resultado$Nucleo
#'
#' @export
func_ELECTRE_resTSuperacion = function(res_electre) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
  alpha = res_electre$alpha
  s1 = func_ELECTRE_resTConcordancia(res_electre)
  s2 = func_ELECTRE_resTDiscordancia(res_electre)
  filas = seq(3,nrow(s1$MIndices),by=3)
  ms1 = s1$MIndices[filas,3:ncol(s1$MIndices)]
  ms1 = data.matrix(ms1)
  ms2 = s2$MIndices[filas,3:ncol(s2$MIndices)]
  ms2 = sapply(ms2,as.integer)
  ms3 =  ms1 * ms2
  md = res_electre$datos
  vd = md[nrow(md),]

  mi1 = ms1
  mi2 = ms2
  mi3 = ms3

  for (i in 1:nrow(mi1)) {
    mf = rbind(mi1[i,],mi2[i,],mi3[i,])
    if (i==1) {
      msal = mf
    } else {
      msal = rbind(msal,mf)
    }
  }
  msal = as.data.frame(msal,stringsAsFactors = FALSE)
  colnames(msal) = paste0("A",1:ncol(msal))

  msal$Inds = rep(c("TC","TD","RSup"),nrow(msal) %/% 3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]

  msal$Alts = rep(paste0("A",1:(ncol(msal)-1)),each=3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  msal0 = msal

  filas = seq(3,nrow(msal),by=3)
  msal_b = msal0[filas,3:ncol(msal0)]
  k=0
  df = matrix(NA,nrow=sum(msal_b),ncol=2)
  for (i in 1:nrow(msal_b)) {
    for (j in 1:ncol(msal_b)) {
      if (msal_b[i,j]==1) {
        k=k+1
        df[k,1]=i
        df[k,2]=j
      }
    }
  }
  df = as.data.frame(df,stringsAsFactors = FALSE)
  colnames(df) = c("De","A")
  df = df[!is.na(df$De),]

  filas = seq(1,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = kableExtra::cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","red","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  filas = seq(2,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = kableExtra::cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","red","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  filas = seq(3,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = kableExtra::cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","blue","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))

  KE_msal = msal %>%
    kableExtra::kbl(booktabs=TRUE,digits = 3,escape = FALSE) %>%
    kableExtra::kable_styling() %>%
    kableExtra::row_spec(0,align="c",color="blue")
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>%
      kableExtra::row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=TRUE)
  }
  KE_msal = KE_msal %>%
    kableExtra::column_spec(1,background = "white",width = "4em",bold = TRUE,color="blue") %>%
    kableExtra::column_spec(2,width = "4em",color="red",bold=TRUE,border_right = TRUE) %>%
    kableExtra::column_spec(3:ncol(msal),width="10em") %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::footnote(general = paste0("ELECTRE: Relacion Superacion: ",
                              "alpha = ",alpha,", ",
                              "vd=(",paste0(vd,collapse=","),")"
    ),general_title = "")

  res = list(
    MIndices = msal0,
    KE = KE_msal,
    Grafo = df,
    Nucleo = res_electre$nucleo_aprox
  )
  return(res)

}


#' Resultados ELECTRE completos
#'
#' Genera todos los resultados de ELECTRE de forma completa: indices,
#' concordancia, discordancia y superacion.
#'
#' @param res_electre Lista resultado de \code{multicriterio.metodoELECTRE_I}.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{MIndices}{Indices de relacion}
#'   \item{TConcordancia}{Tabla de concordancia}
#'   \item{TDiscordancia}{Tabla de discordancia}
#'   \item{TSuperacion}{Tabla de superacion}
#'   \item{Grafo}{Relaciones de dominancia}
#'   \item{Nucleo}{Nucleo aproximado}
#' }
#'
#' @examples
#' tabdec <- matrix(c(20, 13, 15, 30,
#'                     5, 40, 0.3, 0.5,
#'                     0.1, 0.7, 0.9, 0,
#'                     1.3, 4, 2.2, 1), nrow = 4, ncol = 4, byrow = TRUE)
#' rownames(tabdec) <- paste0("A", 1:4)
#' colnames(tabdec) <- paste0("C", 1:4)
#' res_electre <- multicriterio.metodoELECTRE_I(tabdec, c(0.3, 0.2, 0.4, 0.1),
#'                                             0.7, c(15, Inf, 2, Inf))
#' resultado <- func_ELECTRE_Completo(res_electre)
#' resultado$Nucleo
#'
#' @export
func_ELECTRE_Completo = function(res_electre) {

 alpha = res_electre$alpha
 t1 = func_ELECTRE_resMIndices(res_electre)
 t2 = func_ELECTRE_resTConcordancia(res_electre)
 t3 = func_ELECTRE_resTDiscordancia(res_electre)
 t4 = func_ELECTRE_resTSuperacion(res_electre)

 res = list(
   MIndices = t1,
   TConcordancia = t2,
   TDiscordancia = t3,
   TSuperacion = t4,
   Grafo = t4$Grafo,
   Nucleo = res_electre$nucleo_aprox
 )

 return(res)

}


## Funciones para Promethee ----

#' Calculo de la tabla M para Promethee GAIA
#'
#' Calcula la matriz de decision promedio M utilizada en el plano GAIA
#' del metodo Promethee.
#'
#' @param tabdecs.X Matriz numerica de decision (alternativas x criterios).
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Matriz numerica donde cada elemento M(i,j) representa el flujo
#'   promedio de la alternativa i en el criterio j.
#'
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' M <- calculo_tablaM(X, pesos, fpref)
#' M
#'
#' @export
calculo_tablaM = function(tabdecs.X,pesos.criterios,tab.fpref) {
    num.alt = nrow(tabdecs.X);
    num.cri = ncol(tabdecs.X);
    tabla.M = matrix(NA,num.alt,num.cri);
    for (i in 1:num.alt) {
        for (j in 1:num.cri) {
            total = 0
            for (h in 1:num.alt) {
                if (i!=h) {
                    vai = tabdecs.X[i,j];
                    vah = tabdecs.X[h,j];
                    cual = tab.fpref[j,1];
                    qi = tab.fpref[j,2];
                    pi = tab.fpref[j,3];
                    si = tab.fpref[j,4];
                    total = total + fpref.todas(cual,vai,vah,qi,pi,si) - fpref.todas(cual,vah,vai,qi,pi,si)
                }
            }
            tabla.M[i,j] = (1/(num.alt-1)) * total;
        }
    }
    dimnames(tabla.M) = list(rownames(tabdecs.X), colnames(tabdecs.X))
    return(tabla.M);
}


#' Promethee GAIA
#'
#' Aplica el metodo Promethee con analisis GAIA (proyeccion PCA) para
#' visualizar las alternativas en un plano bidimensional.
#'
#' @param tabdecs.X Matriz numerica de decision.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{tabla.M}{Matriz de flujos promedio por criterio}
#'   \item{vflujos.netos}{Vector de flujos netos}
#'   \item{vflujos.netos.ordenados}{Flujos netos ordenados}
#'   \item{alt.ordenadas}{Alternativas ordenadas de mejor a peor}
#'   \item{delta_ProyeccionGAIA}{Porcentaje de varianza explicada por los ejes GAIA}
#'   \item{Plano.GAIA}{Objeto ggplot2 con el plano GAIA}
#' }
#'
#' @examples
#' \dontrun{
#' X <- multicriterio.crea.matrizdecision(
#'   c(10, 20, 5, 8, 15, 12, 12, 18, 7, 9, 14, 11), 4, 3)
#' pesos <- c(0.5, 0.3, 0.2)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' resultado <- multicriterio.metodo.promethee_plano_GAIA_med(X, pesos, fpref)
#' resultado$alt.ordenadas
#' resultado$Plano.GAIA
#' }
#'
#' @export
multicriterio.metodo.promethee_plano_GAIA_med = function(tabdecs.X,pesos.criterios,tab.fpref) {

  if (!requireNamespace("factoextra", quietly = TRUE)) {
    stop("Package \"factoextra\" needed for this function. Please install it with install.packages(\"factoextra\").", call. = FALSE)
  }
    num.alt = nrow(tabdecs.X)
    num.cri = ncol(tabdecs.X)
    tabla.M1 =  calculo_tablaM(tabdecs.X,pesos.criterios,tab.fpref)
    PCA.UNIC = stats::prcomp(tabla.M1, center = TRUE, scale. = TRUE)
    # factoextra/ggpubr emiten un aviso deprecado de ggplot2 (size -> linewidth)
    # ajeno a este paquete; se silencia para no confundir al usuario.
    Plano.GAIA = suppressWarnings(factoextra::fviz_pca_biplot(PCA.UNIC,
                                             geom =c("point", "text"),
                                             repel = TRUE,
                                             col.ind = "#FC4E07",
                                             axes = c(1, 2),
                                             pointsize = 1.5,
                                             title = "Plano GAIA (Promethee II)"))

    v.flujos.netos = rowSums(tabla.M1 * matrix(pesos.criterios,
                                               nrow = num.alt,
                                               ncol = num.cri,
                                               byrow=TRUE))

    vflujos.netos.ordenados = sort(v.flujos.netos, decreasing = TRUE)

    resumen.pca = summary(PCA.UNIC)
    delta_ProyeccionGAIA = resumen.pca$importance[3,2] * 100

    result = list(
        tabla.M = tabla.M1,
        vflujos.netos = v.flujos.netos,
        vflujos.netos.ordenados = vflujos.netos.ordenados,
        alt.ordenadas = names(vflujos.netos.ordenados),
        delta_ProyeccionGAIA = delta_ProyeccionGAIA,
        Plano.GAIA = Plano.GAIA
    )

    return(result)

}


## Promethee Windows ----

#' Nombre de funcion de preferencia Promethee
#'
#' Devuelve el nombre descriptivo de la funcion de preferencia segun su codigo.
#'
#' @param cual Numerico. Codigo de la funcion (1-6).
#'
#' @return Character string con el nombre de la funcion.
#'
#' @examples
#' multicriterio.metodo.promethee_windows_funconversion(1)
#' multicriterio.metodo.promethee_windows_funconversion(3)
#'
#' @export
multicriterio.metodo.promethee_windows_funconversion = function(cual = 1) {
    nombre = switch (cual,
                     "Usual (1)",
                     "U-shape (2)",
                     "V-shape (3)",
                     "Level (4)",
                     "Linear (5)",
                     "Gaussian (6)"
    )
    return(nombre)
}


#' Bloque 01 de Promethee Windows
#'
#' Genera el primer bloque de informacion de Promethee Windows con la
#' configuracion de preferencias.
#'
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param fminmax Vector de character strings opcional con "min" o "max"
#'   para cada criterio.
#'
#' @return Matriz numerica con la configuracion de preferencias.
#'
#' @examples
#' tab.fpref <- data.frame(tipo = c(1, 1), qi = c(0, 0),
#'                         pi = c(5, 3), si = c(1, 1))
#' pesos <- c(0.6, 0.4)
#' multicriterio.metodo.promethee_windows_bloque01(tab.fpref, pesos)
#'
#' @export
multicriterio.metodo.promethee_windows_bloque01 = function(tab.fpref, pesos.criterios, fminmax = NULL) {

    fnum = tab.fpref[,1]
    f01_nb = sapply(fnum, multicriterio.metodo.promethee_windows_funconversion)

    mbloque01 = rbind(pesos.criterios,
                      f01_nb,
                      tab.fpref[,2],
                      tab.fpref[,3],
                      tab.fpref[,4])
    rownames(mbloque01) = c("Pesos", "Funciones Preferencias",
                            "Q: Indiferencia", "P: Preferencia",
                            "S: Gausiano")
    colnames(mbloque01) = paste0("Criterio", 1:ncol(mbloque01))

    if (!is.null(fminmax)) {
        mbloque01 = rbind(fminmax,
                          mbloque01)
        rownames(mbloque01)[1] = "Min/Max"
    }

    return(mbloque01)
}


#' Bloque 02 de Promethee Windows
#'
#' Genera el segundo bloque de informacion con estadisticas descriptivas
#' de la matriz de decision.
#'
#' @param matdecision Matriz numerica de decision.
#' @param fminmax Vector de character strings opcional con "min" o "max".
#'
#' @return Matriz numerica con estadisticas (minimo, maximo, media, desviacion).
#'
#' @examples
#' multicriterio.metodo.promethee_windows_bloque02(
#'   matrix(c(10,20,5,8), nrow=2, byrow=TRUE)
#' )
#'
#' @export
multicriterio.metodo.promethee_windows_bloque02 = function(matdecision, fminmax = NULL) {
    fDesvtipica = function(x) {
        n = length(x)
        res = (sd(x))*(sqrt(n-1)/sqrt(n))
        return(res)
    }
    l1 = list(Minimo = min, Maximo = max, Media = mean, DesvTipica = fDesvtipica)
    l2 = vector("list",length(l1))
    for (i in 1:length(l1)) {
        l2[[i]] = sapply(as.data.frame(matdecision), l1[[i]])
    }
    res = t(sapply(l2, rbind))
    rownames(res) = c("Minimo","Maximo", "Media","Desviacion Tipica")
    colnames(res) = paste0("Criterio",1:ncol(res))
    return(res)
}


#' Promethee Windows
#'
#' Aplica el metodo Promethee Windows que combina Promethee I y II con
#' un formato de presentacion tipo "ventana" con estadisticas.
#'
#' @param matdecision Matriz numerica de decision.
#' @param tab.fpref Matriz de configuracion de funciones de preferencia.
#' @param pesos.criterios Vector numerico con los pesos de los criterios.
#' @param fminmax Vector de character strings opcional con "min" o "max"
#'   para cada criterio.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{Escenario}{Matriz con la configuracion, estadisticas y datos}
#'   \item{Acciones}{DataFrame con los resultados de Promethee (Phi, Phi+, Phi-)}
#' }
#'
#' @examples
#' matdecision <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' tab.fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                         pi = c(5, 3, 2), si = c(1, 1, 1))
#' resultado <- multicriterio.metodo.promethee_windows(
#'   matdecision, tab.fpref, c(0.5, 0.3, 0.2))
#' resultado$Acciones
#'
#' @export
multicriterio.metodo.promethee_windows = function(matdecision, tab.fpref, pesos.criterios, fminmax = NULL) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function. Please install it with install.packages(\"dplyr\").", call. = FALSE)
  }
    res01_i = multicriterio.metodo.promethee_i_med(
        tabdecs.X = matdecision,
        pesos.criterios = pesos.criterios,
        tab.fpref = tab.fpref
    )

    res01_ii = multicriterio.metodo.promethee_ii_med(
        tabdecs.X = matdecision,
        pesos.criterios = pesos.criterios,
        tab.fpref = tab.fpref
    )

    df_res = data.frame(Phi = res01_ii$vflujos.netos,
                        Phi.mas = res01_i$vflujos.ent,
                        Phi.menos = res01_i$vflujos.sal)
    df_res2 = round(df_res,4) %>%
        dplyr::arrange(dplyr::desc(Phi))

    df_res2 = cbind("Rango" = rank(-df_res2$Phi, ties.method = "first"),
                    df_res2)

    if (!is.null(fminmax)) {
        for (i in 1:ncol(matdecision)) {
            if (fminmax[i]=="min") {
                matdecision[,i] = - matdecision[,i]
            }
        }
        ebloque01 = multicriterio.metodo.promethee_windows_bloque01(tab.fpref, pesos.criterios, fminmax)
        ebloque02 = multicriterio.metodo.promethee_windows_bloque02(matdecision)
    } else {
        ebloque01 = multicriterio.metodo.promethee_windows_bloque01(tab.fpref, pesos.criterios)
        ebloque02 = multicriterio.metodo.promethee_windows_bloque02(matdecision)
    }

    eres = rbind(ebloque01, round(ebloque02,2), matdecision)
    res = list(Escenario = eres, Acciones = df_res2)

    return(res)

}


#' Promethee Windows - Tablas HTML
#'
#' Genera tablas kableExtra en formato HTML para Promethee Windows.
#'
#' @param res_promethee_windows Lista resultado de
#'   \code{multicriterio.metodo.promethee_windows}.
#'
#' @return Lista con las tablas formateadas en HTML.
#'
#' @examples
#' \dontrun{
#' matdecision <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' tab.fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                         pi = c(5, 3, 2), si = c(1, 1, 1))
#' res <- multicriterio.metodo.promethee_windows(
#'   matdecision, tab.fpref, c(0.5, 0.3, 0.2))
#' resultado <- multicriterio.metodo.promethee_windows_kableExtra_html(res)
#' resultado$tabAcciones
#' }
#'
#' @export
multicriterio.metodo.promethee_windows_kableExtra_html = function(res_promethee_windows) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
    tab01 = kableExtra::kbl(res_promethee_windows$Escenario) |>
        kableExtra::kable_paper("striped", full_width = FALSE) |>
        kableExtra::pack_rows("Preferencias",1,6) |>
        kableExtra::pack_rows("Estadisticas",7,10) |>
        kableExtra::pack_rows("Evaluaciones",11,nrow(res_promethee_windows$Escenario))

    tab02 = kableExtra::kbl(res_promethee_windows$Acciones) |>
        kableExtra::kable_paper("striped", full_width = FALSE)

    res = list(tabEscenario = tab01,
               tabAcciones = tab02)
}


#' Promethee Windows - Tablas PDF
#'
#' Genera tablas kableExtra en formato PDF para Promethee Windows.
#'
#' @param res_promethee_windows Lista resultado de
#'   \code{multicriterio.metodo.promethee_windows}.
#'
#' @return Lista con las tablas formateadas en PDF.
#'
#' @examples
#' \dontrun{
#' matdecision <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' tab.fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                         pi = c(5, 3, 2), si = c(1, 1, 1))
#' res <- multicriterio.metodo.promethee_windows(
#'   matdecision, tab.fpref, c(0.5, 0.3, 0.2))
#' resultado <- multicriterio.metodo.promethee_windows_kableExtra_pdf(res)
#' resultado$tabAcciones
#' }
#'
#' @export
multicriterio.metodo.promethee_windows_kableExtra_pdf = function(res_promethee_windows) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function. Please install it with install.packages(\"kableExtra\").", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
    tab01 = kableExtra::kbl(res_promethee_windows$Escenario, booktabs = TRUE) |>
        kableExtra::pack_rows("Preferencias",1,6) |>
        kableExtra::pack_rows("Estadisticas",7,10) |>
        kableExtra::pack_rows("Evaluaciones",11,nrow(res_promethee_windows$Escenario))

    tab02 = kableExtra::kbl(res_promethee_windows$Acciones, booktabs = TRUE)

    res = list(tabEscenario = tab01,
               tabAcciones = tab02)
}


#' Promethee Windows - Tablas adaptativas
#'
#' Genera tablas kableExtra adaptativas segun el formato de salida (HTML o PDF).
#'
#' @param res_promethee_windows Lista resultado de
#'   \code{multicriterio.metodo.promethee_windows}.
#'
#' @return Lista con las tablas formateadas.
#'
#' @examples
#' \dontrun{
#' matdecision <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' tab.fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                         pi = c(5, 3, 2), si = c(1, 1, 1))
#' res <- multicriterio.metodo.promethee_windows(
#'   matdecision, tab.fpref, c(0.5, 0.3, 0.2))
#' resultado <- multicriterio.metodo.promethee_windows_kableExtra(res)
#' resultado$tabAcciones
#' }
#'
#' @export
multicriterio.metodo.promethee_windows_kableExtra = function(res_promethee_windows) {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function. Please install it with install.packages(\"knitr\").", call. = FALSE)
  }
    es_html = knitr::is_html_output()
    if (es_html) {
        res = multicriterio.metodo.promethee_windows_kableExtra_html(res_promethee_windows)
    } else {
        res = multicriterio.metodo.promethee_windows_kableExtra_pdf(res_promethee_windows)
    }
    return(res)

}
