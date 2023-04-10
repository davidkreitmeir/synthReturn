# synthReturn <img src="inst/figures/synthReturn.png" align="right" alt="" width="130" />

The `synthReturn` R package implements the synthetic matching method originally suggested by [Acemoglu et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0304405X16300605) and modified by [Kreitmeir et al. (2023)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751162) to estimate the *average treatment effect* $\widehat{\phi}$ of an event on the stock return of all treated firms. The new version of the synthetic matching method by [Kreitmeir et al. (2023)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751162) accommodates *(i)* multiple event dates and *(ii)* deals with missing values directly instead of relying on the assumption that they are equal to 0.

For more details on the empirical framework, please see the [Empirical Framework Section](#empirical-framework) below.

If you end up using this package, please cite our paper:

* Kreitmeir, D., Lane, N., and Raschky, P. A. (2023). The value of names - Civil society, information, and governing multinationals. *SSRN WP 3751162*, https://ssrn.com/abstract=3751162


## Installation

To install the most recent version of the `synthReturn` package from GitHub:
```
# install.packages("devtools")
devtools::install_github("davidkreitmeir/synthReturn")
```

## Short examples

The following is an illustration of the method for a simulated dataset with *(i)* two event-dates and *(ii)* no missing values. For details on the generation of the simulated dataset(s), please see `data-raw/sim_ret.R`.

Let's first get the data ready:
```
library(synthReturn)
# Load data in that comes in the synthReturn package
data(ret_two_evdates)
```
Now, to estimate the *average treatment effect* $\widehat{\phi}$ with the synthetic matching method, we can use the `synthReturn` function:
```
results <- synthReturn(
  data = ret_two_evdates,
    tidname = "treatid",
    cidname = "controlid",
    rname = "ret",
    dname = "date",
    edname = "eventdate",
    estwind = c(-100,-1),
    eventwind = c(0,5),
    estobs_min = 1,
    eventobs_min = 1,
    placebo = TRUE,
    ngroup = 2,
    ndraws = 10
)

results$ate
```

For the **case that returns are missing** for firms either in the treatment or control group, you can set a threshold for the minimum of non-missing trading days during both the *estimation* (`estobs_min`) and *event window* (`eventobs_min`). In this example, I require each firm to have non-missing returns for at least 90% of trading days during both, the *estimation* and *event window* (Note that the default is no missing returns, i.e. 100%).

```

# Load data in that comes in the synthReturn package
data(ret_two_evdates_na) # 5% of all returns missing

results <- synthReturn(
  data = ret_two_evdates,
    tidname = "treatid",
    cidname = "controlid",
    rname = "ret",
    dname = "date",
    edname = "eventdate",
    estwind = c(-100,-1),
    eventwind = c(0,5),
    estobs_min = 0.9,
    eventobs_min = 0.9,
    placebo = TRUE,
    ngroup = 2,
    ndraws = 10
)

results$ate
```


## Empirical Framework

A synthetic match for each company $i$ in the treatment group is found by solving the following optimization problem:
```math
  \underset{\{ w_{j}^{i}\}_{j \in \text{Control group}}}{\arg\min} \underset{t \in \text{Estimation Window}}{\sum \left[ R_{it} - \underset{j \in \text{Control group}}{\sum w_{j}^{i}R_{jt}} \right]^{2}} \quad
  \text{s.t.} \quad  \underset{j \in \text{Control group}}{ \sum w_{j}^{i} = 1}
  \quad \text{and} \quad w_{j}^{i} \geq 0
```
where $R_{it}$ and $R_{jt}$ is the daily return on date $t$ for the treatment, respectively control company and $\{w_{j}^{i*}\}$ is the weight for control firm $j$ in the optimal weighting for firm $i$.

The optimization problem above boils down to a *quadratic programming problem*, as the objective function is quadratic and the two constraints are linear. I.e. the problem can be written as:
```math
       \underset{\mathbf{w} \in \mathbb{R}^{J}}{\arg\min} f \left( \mathbf{w} \right) = \frac{1}{2} \mathbf{w}^\intercal  \mathbf{D} \mathbf{w} - \mathbf{w}^\intercal \mathbf{b} \quad
    \text{s.t.} \quad
    \mathbf{A}_{1}\mathbf{w} = 1 \quad \text{and} \quad
    \mathbf{A}_{2}\mathbf{w} \leq \mathbf{0}
```
where $\mathbf{w} \in \mathbb{R}^{J}$ is a vector containing the optimal weights for each of the $j = 1,...,J$ companies; $\mathbf{D} = \in \mathbb{R}^{J \times J}$ is symmetric and equal to $\mathbf{R}^\intercal \times \mathbf{R}$ with matrix $\mathbf{R} \in \mathbb{R}^{T \times J}$ containing the returns during the *estimation window* of length $T$ for all control companies $J$; $b \in \mathbb{R}^{J}$ and equal to $\mathbf{R}^\intercal \times \mathbf{r}$ with $\mathbf{r} \in \mathbb{R}^{T}$ comprising the returns of the *treated* firm over the *estimation window*; $A_{1} \in \mathbb{R}^{T \times J}$ and $A_{2} \in \mathbb{R}^{J \times J}$ are identity matrices and $\mathbf{0} \in \mathbb{R}^{J}$ a vector of zeros.

Reformulating the optimization problem allows the use the dual method of Goldfarb and Idnani ([1982](https://link.springer.com/chapter/10.1007/BFb0092976), [1983](https://link.springer.com/article/10.1007/BF02591962)) for solving *quadratic programming problem* implemented in `quadprog::solve.QP`.

After finding the optimal weights $w_{j}^{i*}$, the abnormal return of the treated firm $i$ is given by the difference between its return $R_{it}$ and the return for the synthetic firm $w_{j}^{i*}R_{jt}$:
```math
  \widehat{AR}_{it} = R_{it} - \underset{j \in \text{Control group}}{\sum w_{j}^{i*}R_{jt}}
```

The cumulative abnormal return for the period $0$ to $k$ is adjusted for the "goodness" of the synthetic match for all firms in the treatment group:
```math
    \widehat{\phi}\left(0,k\right) = \frac{\underset{i \in \text{Treatment group}}{\sum \frac{\sum_{\tau_{1}=0}^{\tau_{2}} \widehat{AR}_{it}}{\widehat{\sigma_{i}}}}}{\underset{i \in \text{Treatment group}}{\sum \frac{1}{\widehat{\sigma_{i}}}}} \quad
    \text{where} \quad  \widehat{\sigma_{i}} = \sqrt{\frac{\underset{t \in \text{Estimation Window}}{\sum \left(\widehat{AR}_{it}\right)^{2}}}{T}},
```
where $\widehat{\phi}\left(0,k\right)$ is the cumulative effect over the period $\tau_{1}=0$ to $\tau_{2}$ in the *event window*. The treatment effect is, hence, a weighted average of each event-firm specific effect, with greater weight given to the estimated effects for which the synthetic firm tracks the return of the treated company more closely during the *estimation window*.

To draw inference, confidence intervals are constructed by randomly drawing *placebo* treatment groups, as suggested by [Acemoglu et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0304405X16300605). To accommodate multiple event dates `synthRetrun` draws $K \times E$ *placebo* treatment groups of size $N$, where $K$ is the number of random draws at each event date $e$, with the number of (unique) event dates equaling $E$. The cumulative abnormal return effect is significant at the 10\%, 5\%, or 1\% level if the actual estimated treatment effect $\widehat{\phi}$ lies outside of the interval that contains the $\left[5,95\right]$, $\left[2.5,97.5\right]$, or $\left[0.5,99.5\right]$ percentiles of the *placebo* treatment effects $\widehat{\phi}_{\text{placebo}}$.
