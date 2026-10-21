# TDecisionCodigo

Decision theory methods for uncertainty, multicriteria and Bayesian analysis,
as taught in the "Teoría de la Decisión" course (Statistics Degree,
Universidad de Sevilla).

## Installation

Install the released version from CRAN (once published):

```r
install.packages("TDecisionCodigo")
```

Install the development version from GitHub (branch `master` by default):

```r
# install.packages("remotes")
remotes::install_github("calote/TDecisionCodigo")
```

Install from a specific branch (e.g. `feat-documentacion-funciones`):

```r
remotes::install_github("calote/TDecisionCodigo@feat-documentacion-funciones")
# Equivalent form with the ref argument:
# remotes::install_github("calote/TDecisionCodigo",
#                         ref = "feat-documentacion-funciones")
```

Previous releases are available from the Tags page of the GitHub
repository (<https://github.com/calote/TDecisionCodigo/tags>). For
example, version v0.1.0 contains the functions in the legacy format of
separate plain R script files: open its tag and download it by clicking
on the "zip" format.

## Requirements

* R (>= 4.1.0). No hard dependencies beyond base and recommended
  packages: suggested packages are only loaded when the functions that
  need them are called.
* Formatted tables and plots require suggested packages, installed only
  if you use those features:
  * `tinytable`: result tables that adapt to the output format
    (HTML, Typst and LaTeX) and can be exported to PNG, Typst, HTML or
    PDF files. This is the recommended option for Quarto documents
    with `format: typst`.
  * `kableExtra`: result tables for HTML and LaTeX only.
  * `webshot2` plus a Chrome or Chromium browser: table export to PNG
    images (useful to embed tables in LaTeX-generated PDFs).
  * `factoextra` and `ggplot2`: GAIA plane for PROMETHEE.
  * `formattable`: formatted AHP weight tables.
  * `dplyr`: PROMETHEE Windows calculations.
* A LaTeX installation (e.g. TinyTeX) is only needed to compile PDF
  documents and for table export to PDF.

## Overview

* **Uncertainty:** Wald, Optimistic, Hurwicz (with interval analysis),
  Savage, Laplace, Ideal Point and joint summary.
* **Multicriteria:** value functions, Nadir and Promethee homogenisation,
  Analytic Hierarchy Process (three variants, global weights,
  inconsistency), ELECTRE I, PROMETHEE I and II, TOPSIS (classic and
  modified variants), GAIA plane and Arrow-Raymond.
* **Bayesian trees:** joint, marginal and posterior distributions, prior and
  posterior expected values, REII, RER, VEX, VEIP, entropy and channel
  information.

The Promethee Windows tables can be built with either 'kableExtra' (HTML and
LaTeX only) or 'tinytable', which also produces native Typst output and can
export the tables to PNG, Typst, HTML or PDF files:

```r
res <- multicriterio.metodo.promethee_windows(matdecision, tab.fpref, pesos)
tablas <- multicriterio.metodo.promethee_windows_tinytable(res)
tablas$tabEscenario   # adapts to the output format (HTML, Typst or LaTeX)
ficheros <- multicriterio.metodo.promethee_windows_tinytable_exportar(
  res, formato = "png", directorio = ".")
```

## Example

```r
library(TDecisionCodigo)

# Decision under uncertainty
X <- crea.tablaX(c(5, 4, 6, 2, 3, 1, -1, 8, 7, 5, 2, 0),
                 numalternativas = 4, numestados = 3)
criterio.Wald(X, favorable = TRUE)
criterio.Todos(X, favorable = TRUE)

# Bayesian analysis
probs_E <- c(0.3, 0.5, 0.2)
probs_XdE <- matrix(c(0.8, 0.2, 0.4, 0.6, 0.1, 0.9),
                    nrow = 3, byrow = TRUE)
TV <- matrix(c(100, 50, -30, 80, 60, 10), nrow = 2, byrow = TRUE)
func_VEX(TV, probs_E, probs_XdE, favorable = TRUE)

# Multicriteria (classic TOPSIS, all criteria to maximise by default)
Y <- multicriterio.crea.matrizdecision(c(8, 3, 6, 5, 7, 4, 9, 2, 6), 3, 3,
                 v.nombresalt = c("A1", "A2", "A3"))
res <- multicriterio.metodo.topsis(Y, c(0.5, 0.3, 0.2))
res$tabla
multicriterio.metodo.topsis_grafico(res)
```

## Documentation

Full function reference is available in the package manuals
(`help(package = "TDecisionCodigo")`). Worked examples that render to
Typst, HTML and PDF are preserved in `tests/manual/` (excluded from the
built package), e.g. `tests/manual/Multicriterio/testTopsis01.qmd` and
`tests/manual/Multicriterio/testPrometheeWindows03.qmd`.

## Source and bug reports

* Source: <https://github.com/calote/TDecisionCodigo>
* Bug reports: <https://github.com/calote/TDecisionCodigo/issues>

## Teaching use

Legacy teaching scripts used to develop this package are preserved in
`tests/manual/` (excluded from the built package).
