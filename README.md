# synthReturn <img src="inst/figures/synthReturn.png" align="right" alt="" width="130" />


<!-- badges: start -->
<a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License"> </a>
<a href="https://cran.r-project.org/package=synthReturn"><img src="https://www.r-pkg.org/badges/version/synthReturn" alt="Version"> </a>
<!-- badges: end -->



The `synthReturn` R package implements the revised *Synthetic Matching Algorithm* of [Kreitmeir et al. (2025)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751162), building on the original approach of [Acemoglu et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0304405X16300605), to estimate the *cumulative treatment effect* of an event on treated firms’ stock returns. For details on the *Synthetic Matching Algorithm* and the available inference methods, see Section A.2 of the supplementary Online Appendix.

If you end up using this package, please cite the package and our paper:

* Kreitmeir, D., and Düben, C. (2025). synthReturn. R Package Version 1.0.0.

* Kreitmeir, D., Lane, N., and Raschky, P. A. (2025). The value of names - Civil society, information, and governing multinationals., *conditionally accepted at Journal of the European Economic Association*.

## Installation

To install the most recent version of the `synthReturn` package from GitHub:
```
# install.packages("devtools")
devtools::install_github("davidkreitmeir/synthReturn")
```

## Short examples

The following is an illustration of the method for a simulated dataset with two event-dates.

```
library(synthReturn)
# Load data in that comes in the synthReturn package
data(ret_two_evdates)
```

1. We run the synthetic matching algorithm with *permutation* inference.

```
set.seed(123) # set random seed

# Run synthReturn
res.placebo <- synthReturn(
  data = ret_two_evdates,
  unitname = "unit",
  treatname = "treat",
  dname = "date",
  rname = "ret",
  edname = "eventdate",
  estwind = c(-100,-1),
  eventwind = c(0,5),
  estobs_min = 1,
  eventobs_min = 1,
  inference = "permutation",
  correction = FALSE,
  ncontrol_min = 10,
  ndraws = 100,
  ncores = 1
)

# Print result table
print(res.placebo)
```

2. We run the synthetic matching algorithm with a *nonparametric bootstrap* procedure to obtain uncertainty estimates.

```
set.seed(123) # set random seed

# Run synthReturn
res.boot <- synthReturn(
  data = ret_two_evdates,
  unitname = "unit",
  treatname = "treat",
  dname = "date",
  rname = "ret",
  edname = "eventdate",
  estwind = c(-100,-1),
  eventwind = c(0,5),
  estobs_min = 1,
  eventobs_min = 1,
  inference = "bootstrap",
  correction = FALSE,
  ncontrol_min = 10,
  ndraws = 100,
  ncores = 1
)

# Print result table
print(res.boot)
```

3. We make use of the parallelization of `synthRetrun` by setting `ncores = NULL`. The default `ncores = NULL` uses all available cores. In addition, we provide the option `static_scheduling` to set the scheduling type, where `TRUE` (default) implies static scheduling, and `FALSE` dynamic scheduling. Note that the scheduling choice has no effect when `ncores = 1` and in `inference = "permutation"` estimations on Windows machines.

```
set.seed(123) # set random seed

# Run synthReturn
res.parallel <- synthReturn(
  data = ret_two_evdates,
  unitname = "unit",
  treatname = "treat",
  dname = "date",
  rname = "ret",
  edname = "eventdate",
  estwind = c(-100,-1),
  eventwind = c(0,5),
  estobs_min = 1,
  eventobs_min = 1,
  inference = "permutation",
  correction = FALSE,
  ncontrol_min = 10,
  ndraws = 100,
  ncores = NULL,
  static_scheduling = TRUE
)

# Print result table
print(res.parallel)
```

---
Contributors:
  - David H. Kreitmeir ([@davidkreitmeir](https://github.com/davidkreitmeir))
  - Christian Düben ([@cdueben](https://github.com/cdueben))
---
