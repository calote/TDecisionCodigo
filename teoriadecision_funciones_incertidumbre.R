# fichero: teoriadecision_funciones_incertidumbre_nuevo.R ----
## Funciones útiles ----

crea.tablaX = function(vector_matporfilas,numalternativas=3,numestados=4) {
  
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE)
  colnames(X) = paste('e',1:numestados,sep='');
  rownames(X) = paste('d',1:numalternativas,sep='');
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
    PuntoIdeal = min(AltPT);
    Alt_PuntoIdeal = which.min.general(AltPT);
    metodo = 'favorable';
  } else {
    MejoresPT = apply(X,MARGIN=2,min); # desfavorable
    AltPT = rep(0,dim(X)[1])
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
  decopt = c(rep(NA,numestados),
             paste0("d",cri01$AlternativaOptima,collapse = ","),
             paste0("d",cri02$AlternativaOptima,collapse = ","),
             paste0("d",cri03$AlternativaOptima,collapse = ","),
             paste0("d",cri04$AlternativaOptima,collapse = ","),
             paste0("d",cri05$AlternativaOptima,collapse = ","),
             paste0("d",cri06$AlternativaOptima,collapse = ","));
  resultado[nrow(resultado),] = decopt
  ## fin nuevo
  
  return(resultado)
  
}


