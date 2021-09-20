library(knitr)
library(kableExtra)
library(dplyr)
library(stringr)

# Definición de funciones ----------------------------

# #Genera Criterio
# func_genera_Criterio(nalt,Minimo=0,Maximo=1000) 
# #Genera Matriz Valoraciones desde Criterio
# func_conv_a_01_Criterio(vc_Val,Desig=TRUE)  
# #
# #Genera MValoraciones desde número alternativas
# func_genera_Mvaloraciones(nalt,Minimo=0,Maximo=1000,Desig=TRUE) 
# #
# #Genera Matriz de Criterios
# func_genera_MatrizCriterios(nalt,ncri,vMin=rep(0,ncri),vMax=rep(1000,ncri))
# func_cambia_MatrizCriterios_SonBeneficios(MCri,vl_Ben=rep(T,ncol(MCri)))
# #
# func_comprueba_transitiva(A,i,j,k)
# func_comprueba_transitiva_MValoraciones(A1)

# MVcol01 = func_conv_a_01_Criterio(VCri[,1])
# MVcol01
# MVcol02 = func_conv_a_01_Criterio(VCri[,2])
# MVcol02
#MVcol = lapply(as.data.frame(VCri),func_conv_a_01_Criterio)

  #library(qgraph)
  # cuales = c(2,1,6)
  # qgraph(A1)
  # qgraph(A1[cuales,cuales])
  #qgraph(A1[-cuales,-cuales])
  

# Inicio definición de funciones ---------------------

func_genera_Criterio = function(nalt,Minimo=0,Maximo=1000) {
  VCri = floor(runif(nalt,Minimo,Maximo))
  return(VCri)
}

func_conv_a_01_Criterio = function(vc_Val,Desig=TRUE) {
  mSal = matrix(NA,length(vc_Val),length(vc_Val))
  for (i1 in 1:length(vc_Val)) {
    for (i2 in 1:length(vc_Val)) {
      if (Desig) { # >=
        if (vc_Val[i1]>=vc_Val[i2]) {
          mSal[i1,i2] = 1
        } else {
          mSal[i1,i2] = 0
        }
      } else { # >
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
# MVcol01 = func_conv_a_01_Criterio(VCri[,1])
# MVcol01
# MVcol02 = func_conv_a_01_Criterio(VCri[,2])
# MVcol02
#MVcol = lapply(as.data.frame(VCri),func_conv_a_01_Criterio)

func_genera_Mvaloraciones = function(nalt,Minimo=0,Maximo=1000,Desig=TRUE) {
  VCri = func_genera_Criterio(nalt,Minimo,Maximo)
  A1 = func_conv_a_01_Criterio(VCri,Desig)
  colnames(A1) = paste0("a",1:ncol(A1))
  rownames(A1) = colnames(A1)
  return(A1)
}


# set.seed(30)
# for (i in 1:30) {
#   cat(i,"\n")
#   A1b = func_genera_Mvaloraciones(nalt = 10)
#   func_comprueba_transitiva_MValoraciones(A1b)
# }


# Matriz de decisión: alternativas x criterios
func_genera_MatrizCriterios = function(nalt,ncri,vMin=rep(0,ncri),vMax=rep(1000,ncri)) {
  VCri = matrix(rep(NA,ncri*nalt),nalt,ncri)
  for (i in 1:ncri) {
    VCri[,i] = floor(runif(nalt,vMin[i],vMax[i]))
  }
  colnames(VCri) = paste0("C",1:ncol(VCri))
  rownames(VCri) = paste0("a",1:nrow(VCri))
  
  return(VCri)
}

# Matriz de decisión con columnas de beneficios y/o costos convertida a todas beneficios
func_cambia_MatrizCriterios_SonBeneficios = function(MCri,vl_Ben=rep(T,ncol(MCri))) {
    vsigno = ifelse(vl_Ben,1,-1)
    msigno = matrix(vsigno,nrow(MCri),ncol(MCri),byrow=T)
    MCri_sal = MCri * msigno
    return(MCri_sal)

}

#library(qgraph)
#cuales = c(1,2,10)
#cuales = c(1,6,5)
#cuales = c(2,8,1)
#cuales = c(2,1,3)
# cuales = c(2,1,6)
# qgraph(A1)
# qgraph(A1[cuales,cuales])
#qgraph(A1[-cuales,-cuales])


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


func_comprueba_transitiva_MValoraciones = function(A1) {
  
  for (i in 1:ncol(A1)) {
    for (j in 1:ncol(A1)) {
      if (j!=i) {
        for (k in 1:ncol(A1)) {
          if ((j!=k) & (k!=i)) {
            if (!func_comprueba_transitiva(A1,i,j,k)) {
              
              #stop(i,j,k,": no cumplen transitiva")
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


#func_comprueba_transitiva_MValoraciones(A1)
# set.seed(30)
# for (i in 1:30) {
#   cat(i,"\n")
#   A1b = func_genera_Mvaloraciones(nalt = 10)
#   func_comprueba_transitiva_MValoraciones(A1b)
# }


# FUNCIONES ADICIONALES PARA EL MÉTODO ELECTRE  --------------------


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


func_ELECTRE_resMIndices = function(res_electre) {
  
  mi1 = func_ELECTRE_malts_T(res_electre$Imas)
  #mi1
  mi2 = func_ELECTRE_malts_T(res_electre$Iigual)
  #mi2
  mi3 = func_ELECTRE_malts_T(res_electre$Imenos)
  #mi3
  
  
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
    kable(booktabs=T) %>% 
    kable_styling() %>% 
    row_spec(0,align="c",color="blue")
  #colores = rainbow(3)
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>% 
      row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=T)
  }
  KE_msal = KE_msal %>% 
    column_spec(1,background = "white",width = "4em",bold = T,color="blue") %>% 
    column_spec(2,width = "4em",color="red",bold=T,border_right = T) %>% 
    column_spec(3:ncol(msal),width="10em") %>% 
    collapse_rows(columns = 1) %>% 
    footnote(general = "ELECTRE: Conjunto de Índices",general_title = "")
  
  if ( !(knitr::is_html_output() | interactive() ) ) {
    # KE_msal = KE_msal %>% 
    #   as_image()
  }

  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)
}


func_ELECTRE_resTConcordancia = function(res_electre,digitos=4) {
  
  mi1 = res_electre$ind.concordancia
  #mi1
  mi2 = res_electre$ind.concordancia.gorro
  #mi2
  mi3 = res_electre$test.concordancia
  #mi3
  
  
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
  msal[filas,3:ncol(msal)] = cell_spec(ifelse(msal[filas,3:ncol(msal)]>=1,"T","F"),
                                   background = ifelse(msal[filas,3:ncol(msal)]>=1,"blue","white"),
                                   color=ifelse(msal[filas,3:ncol(msal)]>=1,"white","black"))
  
  KE_msal = msal %>% 
    kable(booktabs=T,digits = 3,escape = F,format = "html") %>% 
    kable_styling() %>% 
    row_spec(0,align="c",color="blue")
  #colores = rainbow(3)
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>% 
      row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=T)
  }
  KE_msal = KE_msal %>% 
    column_spec(1,background = "white",width = "4em",bold = T,color="blue") %>% 
    column_spec(2,width = "4em",color="red",bold=T,border_right = T) %>% 
    column_spec(3:ncol(msal),width="10em") %>% 
    collapse_rows(columns = 1) %>% 
    footnote(general = "ELECTRE: Test Concordancia",general_title = "")
  
  if ( !(knitr::is_html_output() | interactive() ) ) {
    # KE_msal = KE_msal %>% 
    #   as_image()
  }
  
  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)
}



func_ELECTRE_resTDiscordancia = function(res_electre) {
  
  #res_electre = elec_p2
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
       #i =2; j=4
       b1 = str_trim(mf1[i,j])
       b2l = as.integer(unlist(str_split(b1,",")))
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
  #msal = round(msal,digits = digitos)
  msal = as.data.frame(msal,stringsAsFactors = F)
  colnames(msal) = paste0("A",1:ncol(msal))
  
  msal$Inds = rep(c("I-","Djk","TD"),nrow(msal) %/% 3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  
  msal$Alts = rep(paste0("A",1:(ncol(msal)-1)),each=3)
  msal = msal[,c(ncol(msal),1:(ncol(msal)-1))]
  msal0 = msal
  
  filas = seq(3,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","blue","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  
  KE_msal = msal %>% 
    kable(booktabs=T,digits = 3,escape = F) %>% 
    kable_styling() %>% 
    row_spec(0,align="c",color="blue")
  #colores = rainbow(3)
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>% 
      row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=T)
  }
  KE_msal = KE_msal %>% 
    column_spec(1,background = "white",width = "4em",bold = T,color="blue") %>% 
    column_spec(2,width = "4em",color="red",bold=T,border_right = T) %>% 
    column_spec(3:ncol(msal),width="10em") %>% 
    collapse_rows(columns = 1) %>% 
    footnote(general = paste0("ELECTRE: Test Discordancia: ",
                              "vd=(",paste0(vd,collapse=","),")"
                              ),general_title = "") 
  if ( !(knitr::is_html_output() | interactive() ) ) {
    # KE_msal = KE_msal %>% 
    #   as_image()
  }
  res = list(
    MIndices = msal0,
    KE = KE_msal
  )
  return(res)
  
}  


func_ELECTRE_resTSuperacion = function(res_electre) {

  alpha = res_electre$alpha
  s1 = func_ELECTRE_resTConcordancia(res_electre)
  s2 = func_ELECTRE_resTDiscordancia(res_electre)
  filas = seq(3,nrow(s1$MIndices),by=3)
  ms1 = s1$MIndices[filas,3:ncol(s1$MIndices)]
  ms1 = data.matrix(ms1)
  ms2 = s2$MIndices[filas,3:ncol(s2$MIndices)]
  ms2 = sapply(ms2,as.integer)  # mejora para R 4.x.x
  #ms2 = data.matrix(ms2)  
  #if (sum(ms2==2)>0) { # data.matrix convierte character a factor y luego numeric --> 1, 2 
  #  ms2 = ms2 - 1
  #} 
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
  #msal = round(msal,digits = digitos)
  msal = as.data.frame(msal,stringsAsFactors = F)
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
  df = as.data.frame(df,stringsAsFactors = F)
  colnames(df) = c("De","A")
  df = df[!is.na(df$De),]
  

  filas = seq(1,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","red","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  filas = seq(2,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","red","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  
  filas = seq(3,nrow(msal),by=3)
  msal[filas,3:ncol(msal)] = cell_spec(ifelse(msal[filas,3:ncol(msal)]=="1","T","F"),
                                       background = ifelse(msal[filas,3:ncol(msal)]=="1","blue","white"),
                                       color=ifelse(msal[filas,3:ncol(msal)]=="1","white","black"))
  
  KE_msal = msal %>% 
    kable(booktabs=T,digits = 3,escape = F) %>% 
    kable_styling() %>% 
    row_spec(0,align="c",color="blue")
  #colores = rainbow(3)
  colores = gray(seq(0.5,0.9,length.out = 3),alpha=0.2)
  for (i in 1:3) {
    KE_msal = KE_msal %>% 
      row_spec(seq(i,nrow(msal),by=3),background=colores[i],align="c",bold=T)
  }
  KE_msal = KE_msal %>% 
    column_spec(1,background = "white",width = "4em",bold = T,color="blue") %>% 
    column_spec(2,width = "4em",color="red",bold=T,border_right = T) %>% 
    column_spec(3:ncol(msal),width="10em") %>% 
    collapse_rows(columns = 1) %>% 
    footnote(general = paste0("ELECTRE: Relación Superación: ",
                              "alpha = ",alpha,", ",
                              "vd=(",paste0(vd,collapse=","),")"
    ),general_title = "")
  
  if ( !(knitr::is_html_output() | interactive() ) ) {
    # KE_msal = KE_msal %>% 
    #   as_image()
  }

  
  res = list(
    MIndices = msal0,
    KE = KE_msal,
    Grafo = df,
    Nucleo = res_electre$nucleo_aprox
  )
  return(res)
  
}  

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
  
