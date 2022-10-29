library(diagram)
# AVISO: antes de usar este fichero, use siempre antes esta llamada
# source("teoriadecision_funciones_multicriterio.R")

# Xmatriznivel1=Xmat.criterios
# Xmatriznivel2
# multicriterio.metodoahp.diagrama(Xmatriznivel1,Xmatriznivel2)

multicriterio.metodoahp.diagrama = function(Xmatriznivel1,Xmatriznivel2) {
  ##Xmatriznivel2 = array(NA,dim=c(num.alt,num.alt,num.cri))
  ##Xmatriznivel2[,,1] =

  resultados = multicriterio.metodoAHP.variante3.completo(Xmatriznivel1,Xmatriznivel2)
  num.criterios = nrow(Xmatriznivel1)
  num.alternativas = dim(Xmatriznivel2)[1]
  num.nodos = 1+num.criterios+num.alternativas
  M = matrix(data=0,nrow=num.nodos,ncol=num.nodos,byrow=T)
  nombres.nodos = c("Objetivo",rownames(Xmatriznivel1),rownames(Xmatriznivel2[,,1]))
  pesos.loc.nivel1 = resultados$pesos.nivel1$valoraciones.ahp
  pesos.loc.nivel2 = resultados$l.pesos.nivel2[[1]]$valoraciones.ahp
  for (i in 2:num.criterios) {
   pesos.loc.nivel2 = c(pesos.loc.nivel2,resultados$l.pesos.nivel2[[i]]$valoraciones.ahp)
  }
  pesos.globales = resultados$pesos.globales
  M[2:(num.criterios+1),1] = round(pesos.loc.nivel1,4)
  M[(num.criterios+2):(num.nodos),2:(num.criterios+1)] = round(pesos.loc.nivel2,4)
  for (i in 1:num.alternativas) {
    nn = 1+num.criterios+i
    M[nn,nn] = round(pesos.globales[i],4)
  }
  dp =plotmat(M, pos = c(1,num.criterios,num.alternativas), name = nombres.nodos, lwd = 1, box.lwd = 2,
              curve = 0, cex.txt = 0.8, box.size = 0.1, box.type = "square",
              box.prop = 0.25, main = "Estructura Jerárquica (AHP)",
              arr.pos = 0.65, arr.lcol = c("red"), arr.col = c("blue"),
              segment.from = 0, segment.to = 1,arr.len=0.2,
              box.cex = 1, arr.type="triangle",dtext=0.1,relsize=1,
              self.lwd=0,self.cex=0.5,
              self.shiftx=-0,self.shifty=-0.05,
              box.col = c("lightblue", rep("green",num.criterios),rep("orange",num.alternativas))
  )
  ##box.col = c("lightblue", "green", "yellow", "orange"))


}

# M <- matrix(nrow = 6, ncol = 6, byrow = TRUE, data = 0)
# nombres.nodos = c("Objetivo","Criterio 1","Criterio 2","Criterio 3","Altern. A","Altern. B")
# #pesos.criterios = 1
# #pesoslocales.nivel2 = 1
# pesos.criterios = c(0.2,0.45,0.35)
# pesoslocales.nivel2 = c(0.4,0.6,0.35,0.65,0.45,0.55)
# M[2:4,1] = pesos.criterios
# M[5:6,2:4] = pesoslocales.nivel2
# #M[5,5]=M[6,6] = 2
# M[5,5] = 0.34
# M[6,6] = 0.66
# ## box.type: "circle", "square", "hexa", "none", "diamond", "multi","ellipse"
# ## arr.type: "triangle", "simple", "circle"
# ## lwd: grosor línea traza los arcos
# ## cex.txt: tamaño fuente etiqueta arcos
# ## dtext: posición separación etiqueta de la flecha (cuanto se despega)
# ## arr.pos: posición de la etiqueta en el arco y de la flecha, de 0 a 1
# ## arr.lcol: color línea traza el arco
# ## arr.col: color del interior de la flecha en arco
# ## arr.len: tamaño de la flecha, de 0 a 1
# ## segment.from: 0 a 1, posición inicial desde que pinta el arco
# ## segment.to: 0 a 1, posición inicial hasta que pinta el arco
# ## curve: curvatura de las líneas, 0 son segmentos rectilíneos
# ## box.prop: zoom sobre la caja, de más estrecha a más amplia
# ## box.lwd: grosor de la caja
# ## box.col: color de relleno de las cajas
# ## box.lcol: color de línea de las cajas
# ## box.cex: tamaño letra en las cajas
# ## txt.col: color del texto en las cajas
# ## pos: o vector que especifica num. elementos en cada fila,
# ##      o matriz de 2 columnas que especifican (x,y) de cada elemento
# ##      o NULL se colocan en círculo
# ## latex: F (expresiones serán interpretadas antes imprimir),
# ##        T (si código latex se imprime)
# ## relsize: factor de escala para el tamaño del grafo
# ## M: matriz cuadrada de coeficientes, que especifica los arcos (filas: to, columnas: desde)
# cairo_ps("ahp_diagrama_cairo.ps")
# cairo_pdf("ahp_diagrama_cairo.pdf")## mejor
# svg("ahp_diagrama.svg")
# pdf(file = "ahp_diagrama.pdf")
# postscript("ahp_diagrama.ps")
# jpeg("ahp_diagrama.jpg",quality=95)
# png("ahp_diagrama.png")
# dp =plotmat(M, pos = c(1,3,2), name = nombres.nodos, lwd = 1, box.lwd = 2,
#         curve = 0, cex.txt = 0.8, box.size = 0.1, box.type = "square",
#         box.prop = 0.25, main = "Estructura Jerárquica (AHP)",
#         arr.pos = 0.65, arr.lcol = c("red"), arr.col = c("blue"),
#         segment.from = 0, segment.to = 1,arr.len=0.2,
#         box.cex = 1, arr.type="triangle",dtext=0.1,relsize=1,
#         self.lwd=0,self.cex=0.5,
#         self.shiftx=-0,self.shifty=-0.05,
#         box.col = c("lightblue", "green", "yellow", "orange"))
# dev.off()
