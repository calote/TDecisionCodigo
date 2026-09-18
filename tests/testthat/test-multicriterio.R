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
