fic_mult = here::here("teoriadecision_funciones_multicriterio.R")
source(file = fic_mult)
fic_mult_u = here::here("teoriadecision_funciones_multicriterio_utiles.R")
source(file = fic_mult_u)



tabdec.X = multicriterio.crea.matrizdecision(c(-80,90,-6,-5.4,-8,5,
                                               -65,58,-2,-9.7,-1,1,
                                               -83,60,-4,-7.2,-4,7,
                                               -40,80,-10,-7.5,-7,10,
                                               -52,72,-6,-2.0,-3,8,
                                               -94,96,-7,-3.6,-5,6),
                                             numalternativas=6,
                                             numcriterios=6,
                                             v.nombresalt=c('A1', 'A2', 'A3', 'A4', 'A5', 'A6'),
                                             v.nombrescri=c('f1', 'f2', 'f3', 'f4','f5','f6'))
tabdec.X
pesos.criterios = c(1/6,1/6,1/6,1/6,1/6,1/6);
#                num.función, qi, pi, si
tab.fpref = matrix(c(2,10,1,0,
                     3,0,30,0,
                     5,0.5,5,0,
                     4,1,6,0,
                     1,0,1,0,
                     6,0,1,5),ncol=4,byrow=T);
tab.fpref


# Promethee I:

res8 = multicriterio.metodo.promethee_i(tabdec.X,pesos.criterios,tab.fpref)
res8

#qgraph::qgraph(res8$tablarelacionsupera)

#Promethee II:

res8b = multicriterio.metodo.promethee_ii(tabdec.X,pesos.criterios,tab.fpref)
res8b

order(res8b$vflujos.netos,decreasing = TRUE)

# Promethee II (con medias):

res8bm = multicriterio.metodo.promethee_ii_med(tabdec.X,pesos.criterios,tab.fpref)
res8bm

# Promethee II (con medias y Plano GAIA)

res8gaia = multicriterio.metodo.promethee_plano_GAIA_med(tabdec.X,pesos.criterios,tab.fpref)
res8gaia
