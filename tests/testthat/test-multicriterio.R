test_that("decision matrix helpers work", {
  X <- multicriterio.crea.matrizdecision(c(100, 15, 7, 40, 50, 200), 2, 3)
  expect_equal(dim(X), c(2, 3))
  H <- multicriterio.homogeneizacion.nadir(X)
  expect_true(all(H >= 0 & H <= 1))
})

test_that("AHP basic variant returns normalized weights", {
  v <- c(1, 2, 6, 9, 1 / 2, 1, 4, 9, 1 / 6, 1 / 4, 1, 3,
         1 / 9, 1 / 9, 1 / 3, 1)
  Xmat <- multicriterio.crea.matrizvaloraciones(v, 4)
  res <- multicriterio.metodoAHP.variante3.basico(Xmat)
  expect_equal(sum(res$valoraciones.ahp), 1, tolerance = 1e-8)
})

test_that("Promethee II runs on tiny example", {
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  pesos <- c(0.5, 0.3, 0.2)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
                      pi = c(5, 3, 2), si = c(1, 1, 1))
  res <- multicriterio.metodo.promethee(X, pesos, fpref, tipo = "II")
  expect_equal(length(res$vflujos.netos), 2)
})

test_that("Arrow-Raymond returns an ordering", {
  Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
                     12, 35, 17, 25, 100, 7,
                     10, 40, 40, 35, 125, 2), nrow = 3, ncol = 6, byrow = TRUE)
  rownames(Mvalor) <- c("A1", "A2", "A3")
  res <- multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor)
  expect_equal(length(res$alternativasordenadas), 3)
})

test_that("complete AHP handles different numbers of criteria and alternatives", {
  nivel1 <- multicriterio.crea.matrizvaloraciones(
    c(1, 2, 1 / 2, 1),
    numalternativas = 2,
    v.nombres.alternativas = c("Coste", "Calidad")
  )
  nivel2 <- array(NA_real_, dim = c(3, 3, 2))
  nivel2[, , 1] <- multicriterio.crea.matrizvaloraciones(
    c(1, 2, 4, 1 / 2, 1, 3, 1 / 4, 1 / 3, 1),
    numalternativas = 3,
    v.nombres.alternativas = c("A", "B", "C")
  )
  nivel2[, , 2] <- multicriterio.crea.matrizvaloraciones(
    c(1, 3, 2, 1 / 3, 1, 1 / 2, 1 / 2, 2, 1),
    numalternativas = 3,
    v.nombres.alternativas = c("A", "B", "C")
  )
  resultado <- multicriterio.metodoAHP.variante3.completo(nivel1, nivel2)
  expect_length(resultado$l.pesos.nivel2, 2)
  expect_length(resultado$pesos.globales, 3)
  expect_equal(sum(resultado$pesos.globales), 1, tolerance = 1e-8)
})

test_that("normalisation handles constant criteria without NaN", {
  X <- matrix(c(1, 10, 2, 10, 4, 10), nrow = 3, byrow = TRUE)
  resultado <- multicriterio.homogeneizacion.nadir(X)
  expect_false(anyNA(resultado))
  expect_equal(resultado[, 2], c(0, 0, 0))
})

test_that("matrix constructors reject incompatible dimensions", {
  expect_error(
    multicriterio.crea.matrizdecision(1:5, numalternativas = 2, numcriterios = 3),
    "compatible"
  )
  expect_error(
    multicriterio.crea.matrizvaloraciones_mej(1:2, numalternativas = 3),
    "diagonal superior"
  )
})

test_that("AHP and PROMETHEE validate numerical inputs", {
  expect_error(
    multicriterio.metodoAHP.variante3.basico(matrix(c(1, 0, 1, 1), 2)),
    "positivos"
  )
  X <- multicriterio.crea.matrizdecision(1:6, 2, 3)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = 0, pi = 1, si = 1)
  expect_error(
    indice.prefmulticriterio(X, c(0.5, 0.5), fpref),
    "compatibles"
  )
})

test_that("Promethee Windows tables can be built with tinytable", {
  skip_if_not_installed("tinytable")
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
                      pi = c(5, 3, 2), si = c(1, 1, 1))
  res <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2))
  tablas <- multicriterio.metodo.promethee_windows_tinytable(res)
  expect_named(tablas, c("tabEscenario", "tabAcciones"))
  expect_true(inherits(tablas$tabEscenario, "tinytable"))
  expect_true(inherits(tablas$tabAcciones, "tinytable"))
})

test_that("Promethee Windows blocks are detected with and without fminmax", {
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
                      pi = c(5, 3, 2), si = c(1, 1, 1))
  res <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2))
  bloques <- multicriterio.metodo.promethee_windows_bloques_escenario(res)
  expect_equal(bloques$npreferencias, 5)
  expect_equal(bloques$nestadisticas, 4)
  expect_equal(bloques$nalternativas, 2)
  expect_length(bloques$etiquetas, nrow(res$Escenario))

  res2 <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2),
                                                 fminmax = c("min", "max", "max"))
  bloques2 <- multicriterio.metodo.promethee_windows_bloques_escenario(res2)
  expect_equal(bloques2$npreferencias, 6)
  expect_equal(bloques2$nestadisticas, 4)
  expect_length(bloques2$etiquetas, nrow(res2$Escenario))
})

test_that("Promethee Windows tables can be exported with tinytable", {
  skip_if_not_installed("tinytable")
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
                      pi = c(5, 3, 2), si = c(1, 1, 1))
  res <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2))
  destino <- file.path(tempdir(), "testPrometheeWindowsTinytable")
  ficheros <- multicriterio.metodo.promethee_windows_tinytable_exportar(
    res, formato = "typ", directorio = destino)
  expect_true(file.exists(ficheros$tabEscenario))
  expect_true(file.exists(ficheros$tabAcciones))
  expect_true(any(grepl("#table", readLines(ficheros$tabEscenario), fixed = TRUE)))
  expect_error(
    multicriterio.metodo.promethee_windows_tinytable_exportar(
      res, formato = "desconocido", directorio = destino)
  )
})

test_that("Promethee Windows tables can be exported as PNG", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("webshot2")
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
                      pi = c(5, 3, 2), si = c(1, 1, 1))
  res <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2))
  destino <- file.path(tempdir(), "testPrometheeWindowsPng")
  ficheros <- multicriterio.metodo.promethee_windows_tinytable_exportar(
    res, formato = "png", directorio = destino)
  expect_true(file.exists(ficheros$tabEscenario))
  expect_true(file.exists(ficheros$tabAcciones))
})

test_that("TOPSIS ranks symmetric alternatives equally", {
  X <- multicriterio.crea.matrizdecision(c(4, 1, 1, 4), 2, 2)
  res <- multicriterio.metodo.topsis(X, c(0.5, 0.5))
  expect_named(res$tabla,
               c("Alternativa", "Separacion_ideal", "Separacion_antiideal",
                 "Cerca_ideal", "Rango"))
  expect_equal(res$cerca_ideal, c(a1 = 0.5, a2 = 0.5), tolerance = 1e-8)
  expect_equal(res$tabla$Rango, 1:2)
})

test_that("TOPSIS handles cost criteria via fminmax", {
  X <- multicriterio.crea.matrizdecision(c(10, 5, 20, 9), 2, 2,
    v.nombresalt = c("A1", "A2"), v.nombrescri = c("precio", "calidad"))
  res <- multicriterio.metodo.topsis(X, c(0.5, 0.5), fminmax = c("min", "max"))
  expect_equal(res$tabla$Alternativa[1], "A1")
  expect_true(all(res$tabla$Cerca_ideal >= 0 & res$tabla$Cerca_ideal <= 1))
})

test_that("TOPSIS variants agree with equal weights and may differ otherwise", {
  X <- multicriterio.crea.matrizdecision(
    c(690, 3.1, 9, 7, 4, 590, 3.9, 7, 6, 10,
      600, 3.6, 8, 8, 7, 620, 3.8, 7, 10, 6,
      700, 2.8, 10, 4, 6, 650, 4, 6, 9, 8), 6, 5)
  w_iguales <- rep(0.2, 5)
  res_clasico <- multicriterio.metodo.topsis(X, w_iguales)
  res_modif <- multicriterio.metodo.topsis_modificado(X, w_iguales)
  expect_identical(res_clasico$tabla$Alternativa, res_modif$tabla$Alternativa)
  w_desiguales <- c(0.6, 0.1, 0.1, 0.1, 0.1)
  res_clasico2 <- multicriterio.metodo.topsis(X, w_desiguales)
  res_modif2 <- multicriterio.metodo.topsis_modificado(X, w_desiguales)
  expect_false(identical(res_clasico2$tabla$Alternativa,
                         res_modif2$tabla$Alternativa))
})

test_that("TOPSIS validates its inputs", {
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  expect_error(
    multicriterio.metodo.topsis(X, c(0.5, 0.5)),
    "pesos"
  )
  expect_error(
    multicriterio.metodo.topsis(X, c(0.5, 0.3, 0.2), fminmax = c("max", "max")),
    "fminmax"
  )
  expect_error(
    multicriterio.metodo.topsis_modificado(X, c(0.5, 0.3, 0.2),
                                            fminmax = c("max", "mid", "max")),
    "fminmax"
  )
})

test_that("TOPSIS tables can be built with tinytable", {
  skip_if_not_installed("tinytable")
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  res <- multicriterio.metodo.topsis(X, c(0.5, 0.3, 0.2))
  tab <- multicriterio.metodo.topsis_tinytable(res)
  expect_true(inherits(tab, "tinytable"))
  expect_error(multicriterio.metodo.topsis_tinytable(list()))
})

test_that("TOPSIS plot runs without error", {
  X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
  res <- multicriterio.metodo.topsis(X, c(0.5, 0.3, 0.2))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  pos <- multicriterio.metodo.topsis_grafico(res, titulo = "Prueba")
  expect_length(pos, 2)
  expect_error(multicriterio.metodo.topsis_grafico(list()))
})

test_that("Arrow-Raymond accepts weights and keeps the unweighted result", {
  Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
                     12, 35, 17, 25, 100, 7,
                     10, 40, 40, 35, 125, 2,
                     15, 40, 20, 20, 35, 4,
                     12, 35, 15, 45, 75, 6), nrow = 5, ncol = 6, byrow = TRUE)
  rownames(Mvalor) <- c("A1", "A2", "A3", "A4", "A5")
  sin_pesos <- multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor)
  con_pesos <- multicriterio.metodoaxiomatico.ArrowRaymond(
    Mvalor, c(10, 10, 0.1, 0.1, 10, 10))
  expect_identical(sin_pesos$alternativasordenadas,
                   multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor, NULL)$alternativasordenadas)
  expect_length(con_pesos$alternativasordenadas, 5)
  expect_equal(func_calcula_matrizclasificacion(Mvalor),
               func_calcula_matrizclasificacion(Mvalor, NULL))
  expect_error(
    multicriterio.metodoaxiomatico.ArrowRaymond(Mvalor, c(1, 2)),
    "pesos"
  )
})

test_that("weighted mean indicator method ranks the alternatives", {
  Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
                     12, 35, 17, 25, 100, 7,
                     10, 40, 40, 35, 125, 2,
                     15, 40, 20, 20, 35, 4,
                     12, 35, 15, 45, 75, 6), nrow = 5, ncol = 6, byrow = TRUE)
  rownames(Mvalor) <- c("A1", "A2", "A3", "A4", "A5")
  res <- multicriterio.metodo.mediaPonderada(Mvalor)
  expect_named(res, c("Puntuaciones", "Rangos_Alternativas",
                      "Alternativas_Ordenadas", "Mvalor_transf_ponderada"))
  expect_length(res$Alternativas_Ordenadas, 5)
  expect_equal(res$Alternativas_Ordenadas[1], "A3")
  expect_equal(unname(res$Rangos_Alternativas), c(5, 3, 1, 4, 2))
  expect_false(anyNA(res$Puntuaciones))
  expect_error(multicriterio.metodo.mediaPonderada(Mvalor, c(1, 2)), "pesos")
})

test_that("weighted mean indicator method tolerates constant criteria", {
  Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
                     12, 35, 17, 25, 100, 7), nrow = 2, ncol = 6, byrow = TRUE)
  Mvalor[, 3] <- 5
  res <- multicriterio.metodo.mediaPonderada(Mvalor)
  expect_false(any(is.nan(res$Puntuaciones)))
  expect_false(anyNA(res$Puntuaciones))
})

test_that("long range table is built for the comparison plots", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
  rownames(rangos) <- c("Coche", "Bus1", "Bus2")
  tabla <- func_tabla_rangos_metodos(rangos)
  expect_named(tabla, c("Alternativas", "Metodos", "Rangos", "SumaRangos"))
  expect_equal(nrow(tabla), 6)
  expect_equal(unique(tabla$SumaRangos[tabla$Alternativas == "Bus1"]), 10)
  expect_error(func_tabla_rangos_metodos(matrix(1:6, nrow = 3)))
})

test_that("multicriteria plots return a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggrepel")
  Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
                     12, 35, 17, 25, 100, 7,
                     10, 40, 40, 35, 125, 2), nrow = 3, ncol = 6, byrow = TRUE)
  rownames(Mvalor) <- c("A1", "A2", "A3")
  rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
  rownames(rangos) <- c("Coche", "Bus1", "Bus2")
  tabla <- func_tabla_rangos_metodos(rangos)
  valores <- setNames(Mvalor[, 1], rownames(Mvalor))
  expect_true(inherits(plot_promethee_I(c(1, 2, 3), c(3, 2, 1)), "ggplot"))
  expect_true(inherits(plot_barras_vector(valores), "ggplot"))
  expect_true(inherits(plot_lineas_rangos(rangos), "ggplot"))
  expect_true(inherits(plot_barras_acumuladas_rangos(tabla), "ggplot"))
  expect_true(inherits(plot_segmentos_rangos(tabla), "ggplot"))
  expect_true(inherits(plot_barras_acumuladas_Mvaloraciones(Mvalor), "ggplot"))
  expect_error(plot_barras_vector(c(1, 2, 3)))
})
