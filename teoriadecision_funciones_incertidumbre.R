# fichero: teoriadecision_funciones_incertidumbre_nuevo.R ----
## Funciones útiles ----

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
  #colnames(X) = paste('e',1:numestados,sep='');
  #rownames(X) = paste('d',1:numalternativas,sep='');
  return(X);

}


# Introducimos los datos en R en forma de matriz:
#   ```{r}
# X = matrix(c(5,4,6,2,3,1,-1,8,7,5,2,0),nrow=4,ncol=3,byrow=TRUE)
# X
# colnames(X)=c('e1','e2','e3')
# rownames(X)=c('d1','d2','d3','d4')
# X
# ```



which.min.general = function(vector) {
  minimo = min(vector);
  res = which(vector == minimo);
  return(res);

}

which.max.general = function(vector) {
  maximo = max(vector);
  res = which(vector == maximo);
  return(res);

}


##which.min.general(c(3,2,8,2,9,2))
##which.min.general(c(3,2,8,1,9,2))

distanciaEuclidea = function(pto1,pto2) {
  return( sqrt( sum( (pto1-pto2)^2 )  ) )
}



criterio.tablaX.ejemplos = function(cual=1) {

  if (cual==2) { ## cual == 2  ## desfav.
    X = crea.tablaX(c(2,12,-3,5,5,-1,0,10,-2),numalternativas = 3,numestados = 3)
  } else if (cual==3) { ## cual == 3  ## desfav.
    X = crea.tablaX(c(125,120,156,60,130,80),numalternativas = 3,numestados = 2)
  } else {  ## cual == 1
    X = crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0),numalternativas = 4,numestados = 3)
  }
  return(X);

}

## Funciones Métodos de Decisión bajo Incertidumbre ----

## Criterio de Wald o Pesimista
criterio.Wald = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltW = apply(X,MARGIN=1,min);
    ##AltW
    Wald = max(AltW);
    Alt_Wald = which.max.general(AltW);
    metodo = 'favorable';
  } else {
    AltW = apply(X,MARGIN=1,max);
    ##AltW
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





criterio.Optimista = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltM = apply(X,MARGIN=1,max);
    ##AltM
    Maximax = max(AltM);
    Alt_Maximax = which.max.general(AltM);
    metodo = 'favorable';
  } else {
    AltM = apply(X,MARGIN=1,min);
    ##AltM
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



## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz = function(tablaX,alfa=0.3,favorable=TRUE) {
  # alfa es un escalar entre 0 y 1 lo obtiene para ese único valor
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

## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz.General = function(tablaX,alfa=0.3,favorable=TRUE) {
  # si alfa es un escalar entre 0 y 1 lo obtiene para ese único valor
  # si alfa es igual a un número mayor que 1, lo usa para obtener cálculos para dividir el rango 0-1
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
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
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
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


# FUNCION : esta funcion nos da los valores de alfa para los que las alternativas cambian

# Entrada: Tabla, favorable (T/F)
# Salida: Intervalo -> Alternativa (óptima para ese intervalo de alfa)
# Autores: Ana Solis, Luca Ricardi y Paula Gutiérrez (Noviembre-2021)

dibuja.criterio.Hurwicz_Intervalos = function(tablaX,favorable=TRUE,mostrarGrafico=TRUE) {
    X = tablaX # renombramos la tabla
    Altmin = apply(X,MARGIN=1,min)      # vector de minimos (por filas)
    Altmax = apply(X,MARGIN=1,max)      # vector de maximos (por filas)
    valfa = seq(from=0,to=1,by=0.05)    # vector de valores para alfa (entre 0 y 1)
    Hurw <- data.frame(Alt_opt = rep(0,length(valfa)),vHurwicz = rep(0,length(valfa)))

    #Alt_opt = rep(0,length(valfa))      # creamos el vector de decisiones (por el criterio de Hurwicz) para cada valor de alfa

    alfaCorte=c()                       # vector que contiene los valores de alfa donde cambian las decisiones
    for (i in 1:length(valfa)) {
        Opt <- criterio.Hurwicz(X, alfa = valfa[i], favorable)
        Hurw[i,] <-  rbind(Opt$AlternativaOptima[[1]],Opt$ValorOptimo) # obtenemos las alternativas para cada alfa
        Alt=c() # Este va a ser el Vector de las alternativas optimas para todos los alfa
        for (i in 1:dim(Hurw)[1]) {
            valrepetidos = duplicated(Hurw$Alt_opt) # Vector de TRUE/FALSE donde los FALSE son los elementos que se repiten
            if (isFALSE(valrepetidos[i])){
                Alt = c(Alt,Hurw$Alt_opt[i]) # Si es falso (si el valor se repite) lo almacenamos en el vector Alt
            }
        }
    }
    # Teniendo el vector de alternativas (Alt) buscamos los puntos de corte de las rectas asociadas a cada alternativa (beneficios)
    # Por ejemplo, la recta que sale de la alternativa a1 y a2 seria:
    #
    #               a1Max *alfa +(1-alfa)*a1Min = a2Max *alfa +(1-alfa)*a2Min
    #
    # Pasando todo a un  miembro e igualando a 0 nos queda:
    #
    #               alfa * (a1Max- a2Max - a1Min + a2Min) + a1Min -a2Min = 0
    #
    # Buscamos ahora los valores de alfa para los que se cortan las rectas asociadas a cada decision
    for (i in 1:(length(Alt)-1)){
        imax = as.numeric(Altmax[Alt[i]])      # maximo asociado a la decision i del vector Alt
        imax1 = as.numeric(Altmax[Alt[i+1]])   # maximo asociado a la decision i+1 del vector Alt
        imin = as.numeric(Altmin[Alt[i]])      # minimo asociado a la decision i del vector Alt
        imin1 = as.numeric(Altmin[Alt[i+1]])   # minimo asociado a la decision i+1 del vector Alt
        if (favorable){
            pCorte = function(alfa) {alfa * (imax-imax1-imin+imin1)+imin-imin1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
        } else {
            # Para el caso de costes (alternativas a1 y a2):
            #
            #               a1Max *(1-alfa) +alfa*a1Min = a2Max *(1-alfa) +alfa*a2Min
            #
            # Pasando todo a un  miembro e igualando a 0 nos queda:
            #
            #               alfa * (a1Min- a2Min - a1Max + a2Max) + a1Max -a2Max = 0
            #
            pCorte = function(alfa) {alfa * (imin-imin1-imax+imax1)+imax-imax1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
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
        colores = rainbow(nn) #aquí es donde estaba el fallo, por lo que salían todas las lineas azules.
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
            legend("bottomright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
            title("Criterio de Hurwicz (favorable - línea discontinua)")
        } else {
            legend("topright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
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




## Savage

criterio.Savage = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    Mejores = apply(X,MARGIN=2,max);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    ##print(Pesos)
    ## Ahora criterio Wald Minimax Pesimista (desfavorable)
    AltWS= apply(Pesos,MARGIN=1,max);
    Savage = min(AltWS);
    Alt_Savage = which.min.general(AltWS);
    metodo = 'favorable';
  } else {
    Mejores = apply(X,MARGIN=2,min);
    temp1 = rep(Mejores,dim(X)[1])
    Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
    Pesos = abs(Mmejores-X);
    ## Ahora criterio Wald Minimax (desfavorable)
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



criterio.Laplace = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    AltL = apply(X,MARGIN=1,mean);
    Laplace = max(AltL) # favorable
    Alt_Laplace = which.max.general(AltL)
    metodo = 'favorable';
  } else {
    AltL = apply(X,MARGIN=1,mean);
    Laplace = min(AltL) # desfavorable
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



criterio.PuntoIdeal = function(tablaX,favorable=TRUE) {

  X = tablaX;
  if (favorable) {
    MejoresPT = apply(X,MARGIN=2,max); # favorable
    AltPT = rep(0,dim(X)[1])
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    ##AltPT
    names(AltPT) = rownames(tablaX)
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'favorable';
  } else {
    MejoresPT = apply(X,MARGIN=2,min); # desfavorable
    AltPT = rep(0,dim(X)[1])
    names(AltPT) = rownames(tablaX)
    for (i in 1:dim(X)[1]) {
      AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
    }
    ##AltPT
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

  ## nuevo
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
  ## fin nuevo

  return(resultado)

}


