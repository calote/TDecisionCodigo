test_that("joint, marginal and posterior distributions are coherent", {
  probs_E <- c(0.3, 0.5, 0.2)
  probs_XdE <- matrix(c(0.8, 0.2,
                        0.4, 0.6,
                        0.1, 0.9), nrow = 3, byrow = TRUE)
  conj <- func_distribucion_conjunta(probs_E, probs_XdE)
  expect_equal(dim(conj), c(3, 2))
  expect_equal(sum(conj), 1, tolerance = 1e-8)

  marg <- func_distribucion_marginal_X(probs_E, probs_XdE)
  expect_equal(sum(marg), 1, tolerance = 1e-8)

  post <- func_distribucion_E_cond_X(probs_E, probs_XdE)
  expect_equal(dim(post), c(3, 2))
  expect_equal(colSums(post), c(1, 1), tolerance = 1e-8)
})

test_that("entropy and channel information are non-negative", {
  probs_E <- c(0.3, 0.5, 0.2)
  probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9),
                      nrow = 3, byrow = TRUE)
  h <- func_entropia(probs_E)
  expect_true(h >= 0)
  info <- func_informacion_canal(probs_E, probs_XdE)
  expect_true(info >= 0)
  coef <- func_coeficiente_redundancia_canal(probs_E, probs_XdE)
  expect_true(coef >= 0 && coef <= 1)
})

test_that("VEX equals REII minus RER", {
  TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
  probs_E <- c(0.3, 0.5, 0.2)
  probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9),
                      nrow = 3, byrow = TRUE)
  reii <- func_REII(TV, probs_E, probs_XdE, favorable = TRUE)
  rer <- func_RER(TV, probs_E, favorable = TRUE)
  vex <- func_VEX(TV, probs_E, probs_XdE, favorable = TRUE)
  expect_equal(vex, reii - rer, tolerance = 1e-8)
})
