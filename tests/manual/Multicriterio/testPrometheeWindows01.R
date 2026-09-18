fic_mult = here::here("teoriadecision_funciones_multicriterio.R")
source(file = fic_mult)
fic_mult_u = here::here("teoriadecision_funciones_multicriterio_utiles.R")
source(file = fic_mult_u)


matdecision = multicriterio.crea.matrizdecision(
    vector_matporfilas = c(-90, 80, -6, 5.4,
                           -58, 65, -2, 9.7,
                           -60, 83, -4, 7.2,
                           -80, 40, -10, 7.5,
                           -72, 52, -6, 2),
    numalternativas = 5, numcriterios = 4,
    v.nombresalt = paste0("e",1:5),
    v.nombrescri = paste0("criterion",1:4) )


tab.fpref01 = matrix(c(
    # func, qi, pi, si
    2, 10,0,0,
    3, 0,25,0,
    5, 1.5,5,0,
    4,3,6,0
), nrow = 4,ncol = 4,byrow = T)

pesos.criterios01 = c(0.20,0.20,0.30,0.30)



(res = multicriterio.metodo.promethee_windows(matdecision, tab.fpref01, pesos.criterios01))

(res = multicriterio.metodo.promethee_windows(matdecision, tab.fpref01, pesos.criterios01, fminmax = c("min","max","min","max") ))

# Ver para pdf: <https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf>


res02 = multicriterio.metodo.promethee_windows_kableExtra_html(res)

res02$tabEscenario

res02$tabAcciones

# La ordenación es:
rownames(res$Acciones)


res02pdf = multicriterio.metodo.promethee_windows_kableExtra_pdf(res)

res02pdf$tabEscenario

res02pdf$tabAcciones


#res02b = multicriterio.metodo.promethee_windows_kableExtra(res)
#res02b$tabEscenario
#res02b$tabAcciones
