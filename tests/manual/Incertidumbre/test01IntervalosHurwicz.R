# Ejemplos para probar

fic_inc = here::here("teoriadecision_funciones_incertidumbre.R")
source(file = fic_inc)

tb07 = crea.tablaX(c(45,45,540,-180,900,-900), numalternativas = 3, numestados = 2)
tb07
dibuja.criterio.Hurwicz_Intervalos(tb07, favorable = T,mostrarGrafico = F)

dibuja.criterio.Hurwicz_Intervalos(tb07, favorable = T,mostrarGrafico = T)
