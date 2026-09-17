# Fichero: teoriadecision_funciones_incertidumbre.R ----
## Funciones para métodos de decisión bajo incertidumbre
## Incluye criterios de Wald, Optimista, Hurwicz, Savage, Laplace y Punto Ideal.


## Funciones útiles ----

#' Crear matriz de decisión
#'
#' Crea una matriz de decisión a partir de un vector de valores, con nombres
#' de alternativas y estados.
#'
#' @param vector_matporfilas Vector numérico con los valores de la matriz
#'   rellenable por filas.
#' @param numalternativas Numérico. Número de alternativas (filas). Si es NULL,
#'   se infiere de numestados.
#' @param numestados Numérico. Número de estados (columnas). Si es NULL,
#'   se infiere de numalternativas.
#' @param nb_alternativas Vector de character strings opcional con los nombres
#'   de las alternativas.
#' @param nb_estados Vector de character strings opcional con los nombres
#'   de los estados.
#'
#' @return Matriz numérica con los datos organizados, con nombres de filas
#'   y columnas asignados.
#'
#' @examples
#' # Crear matriz 4x3
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0),
#'                   numalternativas = 4, numestados = 3)
#' X
#'
#' # Con nombres personalizados
#' crea.tablaX(c(10, 20, 30, 40), numalternativas = 2,
#'             nb_alternativas = c("d1", "d2"), nb_estados = c("e1", "e2"))
#'
#' @export
crea.tablaX = function(vector_matporfilas,numalternativas=NULL,numestados=NULL,nb_alternativas=NULL,nb_estados=NULL) {

  if (is.null(numestados) & is.null(numalternativas)) {
    stop('Es necesario proporcionar numalternativas o numestados');
  }
  if (is.null(numestados)) {
    numestados = length(vector_matporfilas)/numalternativas;
  }
  if (is.null(numalternativas)) {
    numalternativas = length(vector_matporfilas)/numestados;
  }

  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE)

  if (!is.null(nb_alternativas)) {
    if (length(nb_alternativas) != numalternativas) {
      stop('El numero de nombres de alternativas no coincide con el numero de alternativas');
    }
    rownames(X) = nb_alternativas;
  } else {
    rownames(X) = paste('d',1:numalternativas,sep='');
  }
  if (!is.null(nb_estados)) {
    if (length(nb_estados) != numestados) {
      stop('El numero de nombres de estados no coincide con el numero de estados');
    }
    colnames(X) = nb_estados;
  } else {
    colnames(X) = paste('e',1:numestados,sep='');
  }
  return(X);

}


#' Índices de mínimos y máximos generalizados
#'
#' Funciones auxiliares que devuelven todos los índices donde se alcanza
#' el mínimo o máximo de un vector (no solo el primero).
#'
#' @param vector Vector numérico.
#'
#' @return Vector numérico con los índices donde se alcanza el extremo.
#'
#' @examples
#' which.min.general(c(3, 2, 8, 2, 9, 2))  # Devuelve 2, 4, 6
#' which.max.general(c(3, 2, 8, 2, 9, 2))  # Devuelve 5
#'
#' @export
which.min.general = function(vector) {
  minimo = min(vector);
  res = which(vector == minimo);
  return(res);
}

#' @rdname which.min.general
#' @export
which.max.general = function(vector) {
  maximo = max(vector);
  res = which(vector == maximo);
  return(res);
}


#' Distancia euclídea
#'
#' Calcula la distancia euclídea entre dos puntos.
#'
#' @param pto1 Vector numérico con las coordenadas del primer punto.
#' @param pto2 Vector numérico con las coordenadas del segundo punto.
#'
#' @return Valor numérico con la distancia euclídea.
#'
#' @examples
#' distanciaEuclidea(c(0, 0), c(3, 4))  # Devuelve 5
#' distanciaEuclidea(c(1, 2, 3), c(4, 5, 6))
#'
#' @export
distanciaEuclidea = function(pto1,pto2) {
  return( sqrt( sum( (pto1-pto2)^2 )  ) )
}


#' Ejemplos de matrices de decisión
#'
#' Devuelve matrices de ejemplo predefinidas para ilustrar el uso de los
#' criterios de decisión.
#'
#' @param cual Numérico. Indica qué ejemplo devolver (1, 2 o 3).
#'
#' @return Matriz numérica con la tabla de decisión de ejemplo.
#'
#' @examples
#' criterio.tablaX.ejemplos(1)  # Ejemplo 1 (favorable)
#' criterio.tablaX.ejemplos(2)  # Ejemplo 2 (desfavorable)
#' criterio.tablaX.ejemplos(3)  # Ejemplo 3 (desfavorable)
#'
#' @export
criterio.tablaX.ejemplos = function(cual=1) {

  if (cual==2) {
    X = crea.tablaX(c(2,12,-3,5,5,-1,0,10,-2),numalternativas = 3,numestados = 3)
  } else if (cual==3) {
    X = crea.tablaX(c(125,120,156,60,130,80),numalternativas = 3,numestados = 2)
  } else {
    X = crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0),numalternativas = 4,numestados = 3)
  }
  return(X);

}


## Funciones Métodos de Decisión bajo Incertidumbre ----

#' Criterio de Wald (Pesimista)
#'
#' Aplica el criterio de Wald (minimax para beneficios o maximin para costes).
#' Selecciona la alternativa que garantiza el mejor peor caso.
#'
#' @param tablaX Matriz numérica de decisión donde las filas son alternativas
#'   y las columnas estados naturales.
#' @param favorable Lógico. Si TRUE (por defecto), se trata un problema de
#'   beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Wald"}
#'   \item{metodo}{Tipo de problema: "favorable" o "desfavorable"}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{ValorAlternativas}{Vector con el valor peor de cada alternativa}
#'   \item{ValorOptimo}{Valor óptimo obtenido}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.Wald(X, favorable = TRUE)
#'
#' @export
criterio.Wald = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltW = apply(X,MARGIN=1,min);
    Wald = max(AltW);
    Alt_Wald = which.max.general(AltW);
    metodo = 'favorable';
  } else {
    AltW = apply(X,MARGIN=1,max);
    Wald = min(AltW);
    Alt_Wald = which.min.general(AltW);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Wald';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltW;
  resultados$ValorOptimo = Wald;
  resultados$AlternativaOptima = Alt_Wald;

  return(resultados);

}


#' Criterio Optimista (Maximax)
#'
#' Aplica el criterio optimista que selecciona la alternativa con el mejor
#' mejor caso posible.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Optimista"}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{ValorAlternativas}{Vector con el mejor valor de cada alternativa}
#'   \item{ValorOptimo}{Valor óptimo obtenido}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.Optimista(X, favorable = TRUE)
#'
#' @export
criterio.Optimista = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltM = apply(X,MARGIN=1,max);
    Maximax = max(AltM);
    Alt_Maximax = which.max.general(AltM);
    metodo = 'favorable';
  } else {
    AltM = apply(X,MARGIN=1,min);
    Maximax = min(AltM);
    Alt_Maximax = which.min.general(AltM);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Optimista';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltM;
  resultados$ValorOptimo = Maximax;
  resultados$AlternativaOptima = Alt_Maximax;

  return(resultados);


}


#' Criterio de Hurwicz
#'
#' Aplica el criterio de Hurwicz con un coeficiente de optimismo alfa.
#' Combina el mejor y peor caso de cada alternativa ponderadamente.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param alfa Numérico entre 0 y 1. Coeficiente de optimismo. Si alfa=1 se
#'   obtiene el criterio optimista; si alfa=0, el pesimista.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Hurwicz"}
#'   \item{alfa}{Valor de alfa utilizado}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{ValorAlternativas}{Vector con el valor Hurwicz de cada alternativa}
#'   \item{ValorOptimo}{Valor óptimo obtenido}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.Hurwicz(X, alfa = 0.3, favorable = TRUE)
#'
#' @export
criterio.Hurwicz = function(tablaX,alfa=0.3,favorable=TRUE) {
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    AltH = alfa * Altmax + (1-alfa) * Altmin
    Hurwicz = max(AltH)
    Alt_Hurwicz = which.max.general(AltH)
    metodo = 'favorable';
  } else {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    AltH = (1-alfa) * Altmax + alfa * Altmin
    Hurwicz = min(AltH)
    Alt_Hurwicz = which.min.general(AltH)
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfa;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltH;
  resultados$ValorOptimo = Hurwicz;
  resultados$AlternativaOptima = Alt_Hurwicz;

  return(resultados);

}


#' Criterio de Hurwicz General
#'
#' Variante del criterio de Hurwicz que permite evaluar múltiples valores
#' de alfa simultáneamente.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param alfa Numérico. Si alfa <= 1, se evalúa para ese único valor.
#'   Si alfa > 1, se usa como número de puntos en el rango [0, 1].
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Hurwicz"}
#'   \item{alfa}{Valor original de alfa}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{ValorAlternativas}{Vector con los valores Hurwicz}
#'   \item{ValorOptimo}{Vector de valores óptimos para cada alfa}
#'   \item{AlternativaOptima}{Índice(s) de alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' # Un solo valor de alfa
#' criterio.Hurwicz.General(X, alfa = 0.5, favorable = TRUE)
#' # Múltiples valores
#' criterio.Hurwicz.General(X, alfa = 10, favorable = TRUE)
#'
#' @export
criterio.Hurwicz.General = function(tablaX,alfa=0.3,favorable=TRUE) {
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa));
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    for (i in 1:length(valfa)) {
      alfab = valfa[i];
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH);
      Alt_vHurwicz[i] = which.max(vAltH);
      Alt_vHurwicz_g = which.max.general(vAltH);
    }
    metodo = 'favorable';
  } else {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa));
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    for (i in 1:length(valfa)) {
      alfab = valfa[i];
      vAltH = (1-alfab) * Altmax + alfab * Altmin;
      vHurwicz[i] = min(vAltH);
      Alt_vHurwicz[i] = which.min(vAltH);
      Alt_vHurwicz_g = which.min.general(vAltH);
    }
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfa;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = vAltH;
  resultados$ValorOptimo = vHurwicz;
  if (length(valfa)==1) {
    resultados$AlternativaOptima = Alt_vHurwicz_g;
  } else {
    resultados$AlternativaOptima = Alt_vHurwicz;
  }

  return(resultados);

}


#' Representación gráfica del criterio de Hurwicz
#'
#' Dibuja un gráfico que muestra cómo varía el criterio de Hurwicz para
#' cada alternativa en función del coeficiente de optimismo alfa.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return None. Produce un gráfico como efecto secundario.
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' \dontrun{dibuja.criterio.Hurwicz(X, favorable = TRUE)}
#'
#' @export
dibuja.criterio.Hurwicz = function(tablaX,favorable=TRUE) {
  X = tablaX;
  Altmin = apply(X,MARGIN=1,min);
  Altmax = apply(X,MARGIN=1,max);
  valfa = seq(from=0,to=1,by=0.05);
  vHurwicz = rep(0,length(valfa));
  Alt_vHurwicz = rep(0,length(valfa));
  for (i in 1:length(valfa)) {
    alfab = valfa[i];
    if (favorable) {
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH)
    } else {
      vAltH = alfab * Altmin + (1-alfab) * Altmax;
      vHurwicz[i] = min(vAltH)
    }
  }

  x0=0;x1=1;
  y0 = min(Altmin);
  y1 = max(Altmax);
  rg = y1-y0;
  y0=y0-0.1*rg;y1=y1+0.1*rg;
  plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
  nn = length(Altmin);
  colores = rainbow(nn);
  abline(v=0);
  abline(v=1);
  if (favorable) {
    for (i in 1:nn) {
      aa = Altmin[i];
      bb = (Altmax[i] - Altmin[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }
  } else {
    for (i in 1:nn) {
      aa = Altmax[i];
      bb = (Altmin[i] - Altmax[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }
  }
  lines(valfa,vHurwicz,col=rainbow(nn+1)[nn+1],lty=3,lwd=3)
  if (favorable) {
    legend("bottomright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (favorable - línea discontinua)")
  } else {
    legend("topright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (desfavorable - línea discontinua)")
  }

}


#' Criterio de Hurwicz con intervalos de alfa
#'
#' Calcula los intervalos de alfa para los cuales cada alternativa es óptima
#' según el criterio de Hurwicz.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#' @param mostrarGrafico Lógico. Si TRUE, muestra el gráfico de Hurwicz.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{AltOptimas}{Vector con las alternativas óptimas encontradas}
#'   \item{PuntosDeCorte}{Vector con los valores de alfa donde cambia la decisión}
#'   \item{IntervalosAlfa}{Matriz con los intervalos y la alternativa óptima en cada uno}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' resultado <- dibuja.criterio.Hurwicz_Intervalos(X, favorable = TRUE)
#' resultado$IntervalosAlfa
#'
#' @export
dibuja.criterio.Hurwicz_Intervalos = function(tablaX,favorable=TRUE,mostrarGrafico=TRUE) {
    X = tablaX
    Altmin = apply(X,MARGIN=1,min)
    Altmax = apply(X,MARGIN=1,max)
    valfa = seq(from=0,to=1,by=0.05)
    Hurw <- data.frame(Alt_opt = rep(0,length(valfa)),vHurwicz = rep(0,length(valfa)))

    alfaCorte=c()
    for (i in 1:length(valfa)) {
        Opt <- criterio.Hurwicz(X, alfa = valfa[i], favorable)
        Hurw[i,] <-  rbind(Opt$AlternativaOptima[[1]],Opt$ValorOptimo)
        Alt=c()
        for (i in 1:dim(Hurw)[1]) {
            valrepetidos = duplicated(Hurw$Alt_opt)
            if (isFALSE(valrepetidos[i])){
                Alt = c(Alt,Hurw$Alt_opt[i])
            }
        }
    }

    for (i in 1:(length(Alt)-1)){
        imax = as.numeric(Altmax[Alt[i]])
        imax1 = as.numeric(Altmax[Alt[i+1]])
        imin = as.numeric(Altmin[Alt[i]])
        imin1 = as.numeric(Altmin[Alt[i+1]])
        if (favorable){
            pCorte = function(alfa) {alfa * (imax-imax1-imin+imin1)+imin-imin1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]]
            alfaCorte[i] = alfaC
        } else {
            pCorte = function(alfa) {alfa * (imin-imin1-imax+imax1)+imax-imax1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]]
            alfaCorte[i] = alfaC
        }
    }

    if (mostrarGrafico) {
        x0=0;x1=1;
        y0 = min(Altmin);
        y1 = max(Altmax);
        rg = y1-y0;
        y0=y0-0.1*rg;y1=y1+0.1*rg;
        plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
        nn = length(Altmin);
        colores = rainbow(nn)
        abline(v=0);
        abline(v=1);
        if (favorable) {
            for (i in 1:nn) {
                aa = Altmin[i];
                bb = (Altmax[i] - Altmin[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        } else {
            for (i in 1:nn) {
                aa = Altmax[i];
                bb = (Altmin[i] - Altmax[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        }

        lines(valfa,Hurw$vHurwicz,col="green",lty=3,lwd=3)
        abline(v = alfaCorte, col="red")

        if (favorable) {
            legend("bottomright",legend=rownames(X),fill=colores,inset=0.05)
            title("Criterio de Hurwicz (favorable - línea discontinua)")
        } else {
            legend("topright",legend=rownames(X),fill=colores,inset=0.05)
            title("Criterio de Hurwicz (desfavorable - línea discontinua)")
        }
    }

    alfaCorte = round(alfaCorte, 3)
    if (length(alfaCorte)==1){
        Int1=paste("(",0,",",alfaCorte,")")
        Int2=paste("(",alfaCorte,",",1,")")
        Soluciones = cbind(c(Int1,Int2),c(Alt[1],Alt[2]))
    } else {
        Int0=paste("(",0,",",alfaCorte[1],")")
        Int1=paste("(",alfaCorte[length(alfaCorte)],",",1,")")
        Int = ""
        Soluciones= c(Int0, Alt[1])
        for (i in 1:(length(alfaCorte)-1)){
            Int[i] = paste("(",alfaCorte[i],",",alfaCorte[i+1],")")
            Soluciones = rbind(Soluciones,c(Int[i],Alt[i+1]))
        }
        Soluciones = rbind(Soluciones,c(Int1,Alt[length(Alt)]))
    }
    colnames(Soluciones)=c("Intervalo","Alternativa")

    resultados = list();
    resultados$AltOptimas = Alt;
    resultados$PuntosDeCorte = alfaCorte;
    resultados$IntervalosAlfa = Soluciones;
    return(resultados)

}


#' Criterio de Savage (Minimax de arrepentimiento)
#'
#' Aplica el criterio de Savage que minimiza el arrepentimiento máximo.
#' Calcula la matriz de arrepentimientos y selecciona la alternativa con
#' el menor arrepentimiento máximo.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Savage"}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{Mejores}{Vector con los mejores valores por estado}
#'   \item{Pesos}{Matriz de arrepentimientos}
#'   \item{ValorAlternativas}{Vector con el arrepentimiento máximo de cada alternativa}
#'   \item{ValorOptimo}{Valor óptimo obtenido}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.Savage(X, favorable = TRUE)
#'
#' @export
criterio.Savage = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    Mejores = apply(X,MARGIN=2,max);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    AltWS= apply(Pesos,MARGIN=1,max);
    Savage = min(AltWS);
    Alt_Savage = which.min.general(AltWS);
    metodo = 'favorable';
  } else {
    Mejores = apply(X,MARGIN=2,min);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    AltWS= apply(Pesos,MARGIN=1,max);
    Savage = min(AltWS);
    Alt_Savage = which.min.general(AltWS);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Savage';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$Mejores = Mejores;
  resultados$Pesos = Pesos;
  resultados$ValorAlternativas = AltWS;
  resultados$ValorOptimo = Savage;
  resultados$AlternativaOptima = Alt_Savage;

  return(resultados);

}


#' Criterio de Laplace (igual probabilidades)
#'
#' Aplica el criterio de Laplace asumiendo probabilidades iguales para todos
#' los estados. Calcula la media de cada alternativa.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Laplace"}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{ValorAlternativas}{Vector con la media de cada alternativa}
#'   \item{ValorOptimo}{Valor óptimo obtenido}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.Laplace(X, favorable = TRUE)
#'
#' @export
criterio.Laplace = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltL = apply(X,MARGIN=1,mean);
    Laplace = max(AltL)
    Alt_Laplace = which.max.general(AltL)
    metodo = 'favorable';
  } else {
    AltL = apply(X,MARGIN=1,mean);
    Laplace = min(AltL)
    Alt_Laplace = which.min.general(AltL)
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Laplace';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = AltL;
  resultados$ValorOptimo = Laplace;
  resultados$AlternativaOptima = Alt_Laplace;

  return(resultados);

}


#' Criterio del Punto Ideal
#'
#' Selecciona la alternativa más cercana al punto ideal (la mejor en cada
#' criterio), usando distancia euclídea.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{criterio}{Nombre del criterio: "Punto Ideal"}
#'   \item{metodo}{Tipo de problema}
#'   \item{tablaX}{Tabla de decisión original}
#'   \item{Mejores}{Vector con los mejores valores por estado (punto ideal)}
#'   \item{ValorAlternativas}{Vector con las distancias al punto ideal}
#'   \item{ValorOptimo}{Distancia mínima al punto ideal}
#'   \item{AlternativaOptima}{Índice(s) de la(s) alternativa(s) óptima(s)}
#' }
#'
#' @examples
#' X <- crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0), 4, 3)
#' criterio.PuntoIdeal(X, favorable = TRUE)
#'
#' @export
criterio.PuntoIdeal = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    MejoresPT = apply(X,MARGIN=2,max);
    AltPT = rep(0,dim(X)[1])
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    names(AltPT) = rownames(tablaX)
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'favorable';
  } else {
    MejoresPT = apply(X,MARGIN=2,min);
    AltPT = rep(0,dim(X)[1])
    names(AltPT) = rownames(tablaX)
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Punto Ideal';
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$Mejores = MejoresPT;
  resultados$ValorAlternativas = AltPT;
  resultados$ValorOptimo = PuntoIdeal;
  resultados$AlternativaOptima = Alt_PuntoIdeal;

  return(resultados);

}


#' Aplicación de todos los criterios
#'
#' Aplica de forma simultánea los criterios de Wald, Optimista, Hurwicz,
#' Savage, Laplace y Punto Ideal, devolviendo una tabla resumen con los
#' resultados de todos ellos.
#'
#' @param tablaX Matriz numérica de decisión.
#' @param alfa Numérico. Coeficiente de optimismo para Hurwicz. Por defecto 0.3.
#' @param favorable Lógico. Si TRUE, problema de beneficios. Si FALSE, de costes.
#'
#' @return DataFrame con una tabla resumen que incluye:
#'   - Las valoraciones originales para cada estado
#'   - Los valores de cada criterio para cada alternativa
#'   - La alternativa óptima según cada criterio
#'   - Una columna "Veces Óptima" con el conteo de veces que cada alternativa
#'     es seleccionada como óptima
#'
#' @examples
#' X <- criterio.tablaX.ejemplos(1)
#' criterio.Todos(X, favorable = TRUE)
#'
#' @export
criterio.Todos = function(tablaX,alfa=0.3,favorable=TRUE) {

  cri01 = criterio.Wald(tablaX,favorable);
  cri02 = criterio.Optimista(tablaX,favorable);
  cri03 = criterio.Hurwicz(tablaX,alfa,favorable);
  cri04 = criterio.Savage(tablaX,favorable);
  cri05 = criterio.Laplace(tablaX,favorable);
  cri06 = criterio.PuntoIdeal(tablaX,favorable);

  numestados = ncol(tablaX)
  numalterna = nrow(tablaX)

  resultado = cbind(tablaX,cri01$ValorAlternativas,cri02$ValorAlternativas,
                    cri03$ValorAlternativas,cri04$ValorAlternativas,
                    cri05$ValorAlternativas,cri06$ValorAlternativas);

  decopt = c(rep(NA,numestados),cri01$AlternativaOptima[1],
             cri02$AlternativaOptima[1],cri03$AlternativaOptima[1],
             cri04$AlternativaOptima[1],cri05$AlternativaOptima[1],
             cri06$AlternativaOptima[1]);

  resultado = rbind(resultado,decopt);

  colnames(resultado)[numestados+1] = cri01$criterio;
  colnames(resultado)[numestados+2] = cri02$criterio;
  colnames(resultado)[numestados+3] = cri03$criterio;
  colnames(resultado)[numestados+4] = cri04$criterio;
  colnames(resultado)[numestados+5] = cri05$criterio;
  colnames(resultado)[numestados+6] = cri06$criterio;

  if (favorable) {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (fav.)';
  } else {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (Desfav.)';
  }

  resultado = as.data.frame(resultado)
  resultado = format(resultado,digits=4)
  decopt = c(rep('--',numestados),
             paste0(names(cri01$AlternativaOptima),collapse = ","),
             paste0(names(cri02$AlternativaOptima),collapse = ","),
             paste0(names(cri03$AlternativaOptima),collapse = ","),
             paste0(names(cri04$AlternativaOptima),collapse = ","),
             paste0(names(cri05$AlternativaOptima),collapse = ","),
             paste0(names(cri06$AlternativaOptima),collapse = ","));

  contar_alternativas = c(names(cri01$AlternativaOptima),
    names(cri02$AlternativaOptima),
    names(cri03$AlternativaOptima),
    names(cri04$AlternativaOptima),
    names(cri05$AlternativaOptima),
    names(cri06$AlternativaOptima))
  f1 = factor(contar_alternativas, levels = rownames(tablaX))
  conteo = as.data.frame(table(f1))
  ind_mejor = which.max.general(conteo[,2])

  alt_mejor = paste0(as.character(conteo[ind_mejor, 1]), collapse = ",")
  resultado = cbind(resultado, c(conteo[,2],NA))
  colnames(resultado)[ncol(resultado)] = "Veces Optima"
  decopt = c(decopt, alt_mejor)

  resultado[nrow(resultado),] = decopt

  return(resultado)

}
