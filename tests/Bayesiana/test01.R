# se prueba el cálculo de
#fic_bay = here::here("tests","Bayesiana","teoriadecision_funciones_bayesiana.R")
fic_bay = here::here("teoriadecision_funciones_bayesiana.R")
source(file = fic_bay)

# Ejemplo
probs_estados = c(1/3,1/3,1/3) # suponemos que todo es equiprobable
probs_X_cond_estados = matrix(c(1/2, 1/2,
                                1,0,
                                0,1),nrow = 3, ncol = 2,byrow = TRUE)
tv = matrix(c(
    5, -5, -5,
    -5, 5, -5,
    -5, -5, 5
),nrow = 3, ncol = 3,byrow = TRUE)

res2 = func_estudio_arbolbayesiano_completo(probs_estados,
                                            probs_X_cond_estados,
                                            tv, favorable = TRUE)
res2
