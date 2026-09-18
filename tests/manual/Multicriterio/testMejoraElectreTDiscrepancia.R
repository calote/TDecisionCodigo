fic_mult = here::here("teoriadecision_funciones_multicriterio.R")
source(file = fic_mult)
fic_mult_u = here::here("teoriadecision_funciones_multicriterio_utiles.R")
source(file = fic_mult_u)

# Ejemplo 1

t = multicriterio.crea.matrizdecision(
    c(-200,-100,-200,-550,5,
      -5, -50, 0, -30, 7,
      -50, -5, 0, -100, 3,
      -50, -5, 0, -150, 3),
    numalternativas = 4, numcriterios = 5)
crisub = c(0.023,0.063,0.174,0.106,0.64)

pesos = crisub
d = c(100, 20, Inf, Inf, 2)


sol1 = multicriterio.metodoELECTRE_I(t, pesos.criterios = pesos,
                                     nivel.concordancia.minimo.alpha = 0.7,
                                     no.se.compensan = d,
                                     que.alternativas = TRUE)


func_ELECTRE_resTDiscordancia(sol1)

func_ELECTRE_resTConcordancia(sol1)

func_ELECTRE_resTSuperacion(sol1)

func_ELECTRE_Completo(sol1)
