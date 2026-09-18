# se prueba el cálculo de
#fic_bay = here::here("tests","Bayesiana","teoriadecision_funciones_bayesiana.R")
fic_bay = here::here("teoriadecision_funciones_bayesiana.R")
source(file = fic_bay)

# Ejemplo

res3 = func_estudio_arbolbayesiano_completo(
    probs_estados = c(18/37,7/37,3/37,3/37,3/37,3/37),
    probs_X_cond_estados = matrix(c(
        0.5,0.1,0.1,0.1,0.1,0.1,  # los demas equiprobables
        0.1,0.5,0.1,0.1,0.1,0.1,
        0.1,0.1,0.5,0.1,0.1,0.1,
        0.1,0.1,0.1,0.5,0.1,0.1,
        0.1,0.1,0.1,0.1,0.5,0.1,
        0.1,0.1,0.1,0.1,0.1,0.5),nrow=6,ncol=6,byrow=T),
    tv = matrix(c(
        30, 28, 26, 24, 22, 20,
        40, 35, 30, 25, 20, 25,
        25,24.75,24.5,24.25,24, 17.5
    ),nrow=3,ncol=6,byrow=T),
    favorable = FALSE
)
res3
