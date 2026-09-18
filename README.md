# TDecisionCodigo

Decision theory methods for uncertainty, multicriteria and Bayesian analysis,
as taught in the "Teoría de la Decisión" course (Statistics Degree,
Universidad de Sevilla).

## Installation

Install the released version from CRAN (once published):

```r
install.packages("TDecisionCodigo")
```

Install the development version from GitHub:

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

## Overview

* **Uncertainty:** Wald, Optimistic, Hurwicz (with interval analysis),
  Savage, Laplace, Ideal Point and joint summary.
* **Multicriteria:** value functions, Nadir and Promethee homogenisation,
  Analytic Hierarchy Process (three variants, global weights,
  inconsistency), ELECTRE I, PROMETHEE I and II, GAIA plane and
  Arrow-Raymond.
* **Bayesian trees:** joint, marginal and posterior distributions, prior and
  posterior expected values, REII, RER, VEX, VEIP, entropy and channel
  information.

Optional formatted tables and plots require suggested packages
('kableExtra', 'formattable', 'factoextra', 'diagram', 'webshot', among
others). See `?TDecisionCodigo` for details.

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
```

## Documentation

Full function reference is available in the package manuals
(`help(package = "TDecisionCodigo")`).

## Source and bug reports

* Source: <https://github.com/calote/TDecisionCodigo>
* Bug reports: <https://github.com/calote/TDecisionCodigo/issues>

## Teaching use

Legacy teaching scripts used to develop this package are preserved in
`tests/manual/` (excluded from the built package).
