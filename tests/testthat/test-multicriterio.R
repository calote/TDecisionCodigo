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
  res <- multicriterio.metodo.promethee_ii(X, pesos, fpref)
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
