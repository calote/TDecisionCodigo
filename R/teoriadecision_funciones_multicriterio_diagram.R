
#' Diagrama de la estructura jerarquica AHP
#'
#' Genera un diagrama visual de la estructura jerarquica del metodo AHP
#' mostrando los criterios, alternativas y sus pesos asociados.
#'
#' @param Xmatriznivel1 Matriz cuadrada de comparaciones de criterios (nivel 1).
#' @param Xmatriznivel2 Array tridimensional con las matrices de comparaciones
#'   de alternativas para cada criterio (nivel 2).
#'
#' @return None. Produce un diagrama como efecto secundario.
#'
#' @examples
#' \dontrun{
#' mat_cri <- multicriterio.crea.matrizvaloraciones(c(1,2,1/2,1), 2, c("R","Riesgo"))
#' mat_alt1 <- multicriterio.crea.matrizvaloraciones(c(1,3,1/3,1), 2, c("A","B"))
#' mat_alt2 <- multicriterio.crea.matrizvaloraciones(c(1,1/2,2,1), 2, c("A","B"))
#' mat_nivel2 <- array(NA, dim=c(2,2,2))
#' mat_nivel2[,,1] <- mat_alt1
#' mat_nivel2[,,2] <- mat_alt2
#' multicriterio.metodoahp.diagrama(mat_cri, mat_nivel2)
#' }
#'
#' @export
multicriterio.metodoahp.diagrama = function(Xmatriznivel1,Xmatriznivel2) {

  if (!requireNamespace("diagram", quietly = TRUE)) {
    stop("Package \"diagram\" needed for this function. Please install it with install.packages(\"diagram\").", call. = FALSE)
  }
  resultados = multicriterio.metodoAHP.variante3.completo(Xmatriznivel1,Xmatriznivel2)
  num.criterios = nrow(Xmatriznivel1)
  num.alternativas = dim(Xmatriznivel2)[1]
  num.nodos = 1+num.criterios+num.alternativas
  M = matrix(data=0,nrow=num.nodos,ncol=num.nodos,byrow=TRUE)
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
  dp =diagram::plotmat(M, pos = c(1,num.criterios,num.alternativas), name = nombres.nodos, lwd = 1, box.lwd = 2,
              curve = 0, cex.txt = 0.8, box.size = 0.1, box.type = "square",
              box.prop = 0.25, main = "Estructura Jerarquica (AHP)",
              arr.pos = 0.65, arr.lcol = c("red"), arr.col = c("blue"),
              segment.from = 0, segment.to = 1,arr.len=0.2,
              box.cex = 1, arr.type="triangle",dtext=0.1,relsize=1,
              self.lwd=0,self.cex=0.5,
              self.shiftx=-0,self.shifty=-0.05,
              box.col = c("lightblue", rep("green",num.criterios),rep("orange",num.alternativas))
  )

}
