test_that("arboles_aleatorio es reproducible con la misma semilla", {
  a1 <- arboles_aleatorio(seed = 7)
  a2 <- arboles_aleatorio(seed = 7)
  expect_identical(a1$dot, a2$dot)
  expect_identical(a1$mermaid, a2$mermaid)
  expect_identical(a1$rollback$rema, a2$rollback$rema)
})

test_that("arboles_aleatorio no altera el estado previo del generador", {
  set.seed(123)
  antes <- .Random.seed
  arboles_aleatorio(seed = 7)
  expect_identical(.Random.seed, antes)
})

test_that("arboles_aleatorio respeta rangos y suma de probabilidades", {
  a <- arboles_aleatorio(seed = 3, netapas = 3, min_alternativas = 2,
                         max_alternativas = 4, min_resultados = 2,
                         max_resultados = 4, pago_min = 10, pago_max = 50)
  nodos <- a$tree$nodes
  aristas <- a$tree$edges
  for (id in nodos$id[nodos$type == "R"]) {
    p <- aristas$prob[aristas$from == id]
    expect_gte(length(p), 2L)
    expect_lte(length(p), 4L)
    expect_equal(sum(p), 1)
  }
  for (id in nodos$id[nodos$type == "D"]) {
    k <- sum(aristas$from == id)
    expect_gte(k, 2L)
    expect_lte(k, 4L)
  }
  pagos <- a$tree$payoff
  expect_true(all(pagos >= 10 & pagos <= 50))
  expect_true(all(pagos == floor(pagos)))
})

test_that("arboles_aleatorio de una etapa no crea decisiones hijas", {
  a <- arboles_aleatorio(seed = 1, netapas = 1, p_continua = 1)
  expect_equal(sum(a$tree$nodes$type == "D"), 1L)
})

test_that("arboles_aleatorio valida parametros y tope de nodos", {
  expect_error(arboles_aleatorio(netapas = 0), "netapas")
  expect_error(arboles_aleatorio(min_alternativas = 4, max_alternativas = 2),
               "Alternativas")
  expect_error(arboles_aleatorio(p_continua = 2), "p_continua")
  expect_error(arboles_aleatorio(seed = 1, netapas = 5, p_continua = 1,
                                 max_nodos = 3), "max_nodos")
})

test_that("print.arboles_aleatorio resume sin error", {
  a <- arboles_aleatorio(seed = 7)
  out <- capture.output(print(a))
  expect_true(any(grepl("REMA", out)))
  expect_true(any(grepl("semilla 7", out)))
})

test_that("arboles_aleatorio respeta mark_starred (apagado por defecto)", {
  a <- arboles_aleatorio(seed = 7)
  expect_false(grepl("fef9c3", a$dot, fixed = TRUE))
  expect_false(grepl(" *", a$dot, fixed = TRUE))
  b <- arboles_aleatorio(seed = 7, mark_starred = TRUE)
  expect_true(grepl("fef9c3", b$dot, fixed = TRUE))
  expect_true(grepl(" *", b$dot, fixed = TRUE))
})
