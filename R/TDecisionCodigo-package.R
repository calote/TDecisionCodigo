#' TDecisionCodigo: Decision Theory Methods
#'
#' Tools for decision theory as taught in statistics degrees, covering
#' classical criteria under uncertainty, multicriteria techniques
#' (Analytic Hierarchy Process, ELECTRE, PROMETHEE, Arrow-Raymond) and
#' Bayesian decision trees with value-of-information and entropy measures.
#'
#' @section Main topics:
#' \itemize{
#'   \item Decision under uncertainty: Wald, Optimistic, Hurwicz, Savage,
#'     Laplace, Ideal Point.
#'   \item Multicriteria: value functions, homogenisation, AHP variants,
#'     ELECTRE I, PROMETHEE I and II, GAIA plane, Arrow-Raymond.
#'   \item Bayesian trees: joint, marginal and posterior distributions,
#'     prior and posterior expected values, REII, RER, VEX, VEIP,
#'     entropy and channel information.
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom stats prcomp runif sd uniroot
#' @importFrom graphics plot abline lines legend title
#' @importFrom grDevices rainbow gray
#' @importFrom utils data
"_PACKAGE"

## quiets concerns of R CMD check re: NSE variables
utils::globalVariables(c("Phi", ".data"))
