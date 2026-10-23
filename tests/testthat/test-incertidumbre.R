test_that("crea.tablaX builds correct matrix", {
  X <- crea.tablaX(c(5, 4, 6, 2, 3, 1), numalternativas = 2, numestados = 3)
  expect_equal(dim(X), c(2, 3))
  expect_equal(rownames(X), c("d1", "d2"))
  expect_equal(colnames(X), c("e1", "e2", "e3"))
})

test_that("Wald criterion works (favorable)", {
  X <- crea.tablaX(c(5, 4, 6, 2, 3, 1, -1, 8, 7, 5, 2, 0),
                   numalternativas = 4, numestados = 3)
  res <- criterio.Wald(X, favorable = TRUE)
  expect_equal(res$criterio, "Wald")
  expect_true(is.numeric(res$ValorOptimo))
  expect_true(length(res$AlternativaOptima) >= 1)
})

test_that("Savage and Laplace run", {
  X <- criterio.tablaX.ejemplos(1)
  s <- criterio.Savage(X, favorable = TRUE)
  l <- criterio.Laplace(X, favorable = TRUE)
  expect_equal(s$criterio, "Savage")
  expect_equal(l$criterio, "Laplace")
})

test_that("Hurwicz intervals run without plot", {
  X <- criterio.tablaX.ejemplos(1)
  res <- dibuja.criterio.Hurwicz_Intervalos(X, favorable = TRUE,
                                            mostrarGrafico = FALSE)
  expect_true(!is.null(res$IntervalosAlfa))
  expect_true(!is.null(res$PuntosDeCorte))
})

test_that("Hurwicz intervals handle uniroot failure (issue #22)", {
  # Alternativas con mismo rango (max-min igual) que no se cruzan en [0,1]:
  # antes uniroot lanzaba error al no haber cambio de signo.
  X <- matrix(c(5, 10, 8, 13, 3, 7), nrow = 3, ncol = 2, byrow = TRUE)
  rownames(X) <- c("A1", "A2", "A3")
  res <- dibuja.criterio.Hurwicz_Intervalos(X, favorable = TRUE,
                                            mostrarGrafico = FALSE)
  expect_true(!is.null(res$IntervalosAlfa))
  expect_true(!is.null(res$PuntosDeCorte))
  expect_equal(ncol(res$IntervalosAlfa), 2)
  expect_equal(colnames(res$IntervalosAlfa), c("Intervalo", "Alternativa"))
})

test_that("Hurwicz intervals handle single optimal alternative", {
  # Una alternativa domina en todo [0,1]: no hay punto de corte.
  X <- matrix(c(10, 20, 5, 15), nrow = 2, ncol = 2, byrow = TRUE)
  rownames(X) <- c("A1", "A2")
  res <- dibuja.criterio.Hurwicz_Intervalos(X, favorable = TRUE,
                                            mostrarGrafico = FALSE)
  expect_equal(nrow(res$IntervalosAlfa), 1)
  expect_equal(unname(res$IntervalosAlfa[1, "Intervalo"]), "(0, 1)")
  expect_equal(unname(res$IntervalosAlfa[1, "Alternativa"]), "1")
})
