# TDecisionCodigo 0.2.0

## New features

* New functions `multicriterio.metodo.topsis()` and
  `multicriterio.metodo.topsis_modificado()` implement the classic and
  modified TOPSIS methods, with presentation helpers
  `multicriterio.metodo.topsis_tinytable()` (native Typst, HTML and LaTeX
  tables) and `multicriterio.metodo.topsis_grafico()` (relative-closeness
  bar chart).
* New functions `multicriterio.metodo.promethee_windows_tinytable()` and
  `multicriterio.metodo.promethee_windows_tinytable_exportar()` build the
  Promethee Windows tables with the 'tinytable' package. The objects returned
  by the first one adapt automatically to the output format (HTML, Typst or
  LaTeX) and the second one writes the tables to PNG, Typst, HTML or PDF
  files.

## Development changes

* `multicriterio.metodoAHP.variante3.completo()` now allocates the list of
  second-level weights according to the number of criteria, so AHP hierarchies
  with different numbers of criteria and alternatives are handled correctly.
* The row groups of the Promethee Windows scenario table ("Preferencias",
  "Estadisticas" and "Evaluaciones") are now computed from the actual content
  of the table, so they are also correct when the `fminmax` argument is not
  used (in that case the preferences block has five rows instead of six).

# TDecisionCodigo 0.1.0

## New features

* First CRAN-ready release as an 'R' package.
* Decision under uncertainty: Wald, Optimistic, Hurwicz (including interval
  analysis), Savage, Laplace, Ideal Point and joint summary.
* Multicriteria: value-function construction, Nadir and Promethee
  homogenisation, Analytic Hierarchy Process (three variants, global weights,
  inconsistency), ELECTRE I (two variants), PROMETHEE I and II (sum and mean
  flows), GAIA plane and Arrow-Raymond axiomatic method.
* Bayesian trees: joint, marginal and posterior distributions, prior and
  posterior expected values, REII, RER, VEX, VEIP, entropy and channel
  information, complete benefit/cost and entropy studies.
* Full roxygen2 documentation with runnable examples and 'testthat' coverage
  for core calculations.
