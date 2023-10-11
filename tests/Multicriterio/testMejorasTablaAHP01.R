fic_mult = here::here("teoriadecision_funciones_multicriterio.R")
source(file = fic_mult)

# Ejercicio 2.6


tb0601 = multicriterio.crea.matrizvaloraciones_mej(c(7,9,
                                                     3),
                                                   numalternativas = 3,
                                                   v.nombres.alternativas = c("Costo","Confiabilidad","Plazo Entrega"))
tb0602a = multicriterio.crea.matrizvaloraciones_mej(c(1/3,6,
                                                      8),3,c("A","B","C"))

tb0602b = multicriterio.crea.matrizvaloraciones_mej(c(6,2,
                                                      1/3),3,c("A","B","C"))

tb0602c = multicriterio.crea.matrizvaloraciones_mej(c(8,1,
                                                      1/8),3,c("A","B","C"))

tb.nivel01 = tb0601
l_tb.nivel02 = list(
    tb0602a,
    tb0602b,
    tb0602c
)


lres206 = multicriterio.metodoAHP.pesosglobales_formattable(tb.nivel01, l_tb.nivel02,
                                                            que.variante = 1)

names(lres206)

lres206$tb01
lres206$tb02

# con la opción 3, hay pequeñas modificaciones


# Tabla de pesos locales:
#lres$tb02sal # funcionaría en html solamente
export_formattable(lres206$tb02sal, file = "table206rb.png")



# Tabla de aportaciones:
#lres$tb01sal # funcionaría en html solamente
export_formattable(lres206$tb01sal, file = "table206ra.png")



