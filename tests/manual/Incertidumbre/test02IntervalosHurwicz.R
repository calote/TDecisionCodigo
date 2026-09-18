# Ejemplos para probar

fic_inc = here::here("teoriadecision_funciones_incertidumbre.R")
source(file = fic_inc)

tb05 = crea.tablaX(c(125,120,156,60,130,80), numalternativas = 3, numestados = 2)
tb05
dibuja.criterio.Hurwicz_Intervalos(tb05, favorable = F,mostrarGrafico = F)

dibuja.criterio.Hurwicz_Intervalos(tb05, favorable = F,mostrarGrafico = T)
