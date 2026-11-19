test_that("arboles: rollback del 3.1 da REMA 12200", {
  t31 <- arb_D("ND1",
    A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000))),
    B = arb_R("NR2", w1 = arb_P(0.6, arb_T(11500)), w2 = arb_P(0.4, arb_T(10500))))
  rb <- arboles_rollback(arboles_compile_tree(t31))
  expect_equal(unname(rb$rema), 12200)
  expect_equal(unname(rb$values[["NR1"]]), 12200)
  expect_equal(unname(rb$values[["NR2"]]), 11100)
  expect_equal(rb$optimal$ND1, "NR1")
})

test_that("arboles: rollback con azar anidado (3.8)", {
  t38 <- arb_D("ND1",
    A1 = arb_T(100),
    A2 = arb_R("NR1",
      AC = arb_P(0.4, arb_R("NR2", AA = arb_P(0.5, arb_T(120)),
                                      ES = arb_P(0.3, arb_T(90)),
                                      RE = arb_P(0.2, arb_T(70)))),
      `no AC` = arb_P(0.6, arb_R("NR3", AA = arb_P(0.5, arb_T(150)),
                                          ES = arb_P(0.3, arb_T(110)),
                                          RE = arb_P(0.2, arb_T(80))))))
  rb <- arboles_rollback(arboles_compile_tree(t38))
  expect_equal(unname(rb$rema), 114.8)
  expect_equal(unname(rb$values[["NR2"]]), 101)
  expect_equal(unname(rb$values[["NR3"]]), 124)
})

test_that("arboles: rollback secuencial en 3 etapas (3.9)", {
  d3 <- arb_D("D3",
    a3 = arb_R("C3", A3 = arb_P(0.5, arb_T(6)), F3 = arb_P(0.5, arb_T(-3))),
    b3 = arb_T(4))
  t39 <- arb_D("D1",
    a1 = arb_R("C1",
      A1 = arb_P(0.5, arb_D("D2",
        a2 = arb_R("C2", A2 = arb_P(0.5, d3), F2 = arb_P(0.5, arb_T(-2))),
        b2 = arb_T(2))),
      F1 = arb_P(0.5, arb_T(-1))),
    b1 = arb_T(0))
  rb <- arboles_rollback(arboles_compile_tree(t39))
  expect_equal(unname(rb$rema), 0.5)
  expect_equal(unname(rb$values[["C3"]]), 1.5)
  expect_equal(unname(rb$values[["D3"]]), 4)
  expect_equal(unname(rb$values[["D2"]]), 2)
})

test_that("arboles: from_dataframes coincide con el DSL", {
  nd <- data.frame(id = c("D1", "NR1", "t1", "t2"),
                   type = c("D", "R", "T", "T"),
                   stringsAsFactors = FALSE)
  ed <- data.frame(from = c("D1", "NR1", "NR1"),
                   to = c("NR1", "t1", "t2"),
                   label = c("A", "w1", "w2"),
                   prob = c(NA, 0.6, 0.4),
                   stringsAsFactors = FALSE)
  tr <- arboles_from_dataframes(nd, ed, c(t1 = 15000, t2 = 8000))
  rb <- arboles_rollback(tr)
  expect_equal(unname(rb$rema), 15000 * 0.6 + 8000 * 0.4)
})

test_that("arboles: la validación detecta probabilidades que no suman 1", {
  t <- arb_D("ND1",
    A = arb_R("NR1", w1 = arb_P(0.6, arb_T(1)), w2 = arb_P(0.5, arb_T(2))))
  expect_error(arboles_compile_tree(t), "suman")
})

test_that("arboles: decision_table calcula E, REMA, REIP y VEIP", {
  m <- matrix(c(15000, 11500, 8000, 10500), nrow = 2, byrow = FALSE)
  dt <- arboles_decision_table(m, c(0.6, 0.4))
  expect_equal(unname(dt$E), c(12200, 11100))
  expect_equal(dt$best, 1L)
  expect_equal(unname(dt$rema), 12200)
  expect_equal(unname(dt$reip), 13200)
  expect_equal(unname(dt$veip), 1000)
})

test_that("arboles: decision_table minimiza con maximize = FALSE", {
  m <- matrix(c(10, 10, 15, 30, 15, 17.5, 16, 35, 20, 15, 19, 18),
              nrow = 4, byrow = FALSE)
  dt <- arboles_decision_table(m, c(0.3, 0.4, 0.3), maximize = FALSE)
  expect_equal(dt$best, 2L)
  expect_equal(unname(dt$rema), 14.5)
})

test_that("arboles: to_dot y to_mermaid generan código válido", {
  tr <- arboles_compile_tree(
    arb_D("ND1", A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)),
                                    w2 = arb_P(0.4, arb_T(8000)))))
  dot <- arboles_to_dot(tr, title = "t")
  expect_true(grepl("digraph dtree", dot, fixed = TRUE))
  expect_true(grepl("NR1", dot, fixed = TRUE))
  mm <- arboles_to_mermaid(tr, title = "t")
  expect_true(grepl("flowchart LR", mm, fixed = TRUE))
  expect_true(grepl("linkStyle", mm, fixed = TRUE))
})

test_that("arboles: draw_tree a SVG si hay Graphviz", {
  testthat::skip_if_not(nzchar(Sys.which("dot")), "Graphviz no instalado")
  tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
  f <- tempfile(fileext = ".svg")
  arboles_draw_tree(tr, f)
  expect_true(file.exists(f))
  expect_true(any(grepl("<svg", readLines(f, warn = FALSE), fixed = TRUE)))
})

test_that("arboles: draw_tree deduce el formato de la extension", {
  testthat::skip_if_not(nzchar(Sys.which("dot")), "Graphviz no instalado")
  tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
  f <- tempfile(fileext = ".png")
  testthat::expect_no_warning(arboles_draw_tree(tr, f))
  # PNG real: firma de 8 bytes %89PNG\r\n\032\n
  magic <- readBin(f, "raw", n = 8)
  expect_equal(magic, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
})

test_that("arboles: draw_tree avisa si formato y extension no coinciden", {
  testthat::skip_if_not(nzchar(Sys.which("dot")), "Graphviz no instalado")
  tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
  f <- tempfile(fileext = ".png")
  expect_warning(arboles_draw_tree(tr, f, format = "svg"), "no coincide")
  expect_true(any(grepl("<svg", readLines(f, warn = FALSE), fixed = TRUE)))
})

test_that("arboles: draw_mermaid solo guarda .mmd sin mmdc", {
  tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
  f <- tempfile(fileext = ".png")
  out <- arboles_draw_mermaid(tr, f, format = "mmd")
  expect_true(file.exists(out))
  expect_true(any(grepl("flowchart", readLines(out, warn = FALSE), fixed = TRUE)))
})

test_that("arboles: modo enunciado (sin valores ni óptimo)", {
  tr <- arboles_compile_tree(arb_D("ND1",
    A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000)))))
  d <- arboles_to_dot(tr, show_values = FALSE, mark_optimal = FALSE)
  expect_false(grepl("\u03bc", d, fixed = TRUE))
  expect_false(grepl("red", d, fixed = TRUE))
  expect_false(grepl("15000 *", d, fixed = TRUE))
  expect_true(grepl("NR1", d, fixed = TRUE))
  expect_true(grepl("15000", d, fixed = TRUE))  # los pagos son datos: se quedan
  m <- arboles_to_mermaid(tr, show_values = FALSE, mark_optimal = FALSE)
  expect_false(grepl("\u03bc", m, fixed = TRUE))
  expect_false(grepl("linkStyle", m, fixed = TRUE))
})
