## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Pedro Luis Luque Calvo <calvo@us.es>'

  New submission.

## Method references

There are no single published references describing all methods in this
package. The package implements standard textbook methodology for decision
theory (decision under uncertainty, Analytic Hierarchy Process, ELECTRE,
PROMETHEE and Bayesian decision trees) as taught in statistics degrees.
No new statistical methodology is introduced.

## Notes for reviewers

* Heavy optional functionality depending on 'formattable', 'webshot',
  'factoextra', 'diagram', 'kableExtra' and related packages is placed in
  Suggests and guarded with `requireNamespace()`; examples requiring
  external software (e.g. PhantomJS via 'webshot') use `\dontrun{}`.
* Legacy teaching scripts are preserved in `tests/manual/` and excluded from
  the build via `.Rbuildignore`; `tests/testthat/` contains fast,
  deterministic unit tests.
