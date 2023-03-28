# synthReturn <img src="inst/figures/synthReturn.png" align="right" alt="" width="130" />

R package for the synthetic matching method originally suggested by [Acemoglu et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0304405X16300605) and modified by [Kreitmeir et al. (2023)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751162) to accommodate *(i)* multiple event dates and *(ii)* missing values.

## Installation

```
devtools::install_github("davidkreitmeir/synthReturn")
```

## Empirical Framework

A synthetic match for each company $i$ in the treatment group is found by solving the following optimization problem:
```math
  \underset{\{ w_{j}^{i}\}_{j \in \text{Control group}}}{\arg\min} \underset{t \in \text{Estimation Window}}{\sum \left[ R_{it} - \underset{j \in \text{Control group}}{\sum w_{j}^{i}R_{jt}} \right]^{2}} \quad
  \text{s.t.} \quad  \underset{j \in \text{Control group}}{ \sum w_{j}^{i} = 1}
  \quad \text{and} \quad w_{j}^{i} \geq 0
```
where $R_{it}$ and $R_{jt}$ is the daily return on date $t$ for the treatment, respectively control company and $\{w_{j}^{i*}\}$ is the weight for control firm $j$ in the optimal weighting for firm $i$.

The aforementioned optimization problem boils down to a *quadratic programming problem*, as the objective function is quadratic and the two constraints are linear. I.e. the problem can be written as:
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

The cumulative abnormal return for the period $0$ to $k$ adjusted for the ``goodness'' of the synthetic match is given by:
```math
    \widehat{\phi}\left(0,k\right) = \frac{\underset{i \in \text{Treatment group}}{\sum \frac{\sum_{\tau_{1}=0}^{\tau_{2}} \widehat{AR}_{it}}{\widehat{\sigma_{i}}}}}{\underset{i \in \text{Treatment group}}{\sum \frac{1}{\widehat{\sigma_{i}}}}} \quad
    \text{where} \quad  \widehat{\sigma_{i}} = \sqrt{\frac{\underset{t \in \text{Estimation Window}}{\sum \left(\widehat{AR}_{it}\right)^{2}}}{T}},
```
where $\widehat{\phi}\left(0,k\right)$ is the cumulative effect over the period $\tau_{1}=0$ to $\tau_{2}$ in the *event window*.

To draw inference, one constructs confidence intervals by randomly drawing *placebo* treatment groups, as suggested by [Acemoglu et al. (2016)](https://www.sciencedirect.com/science/article/abs/pii/S0304405X16300605). To accommodate multiple event dates `synthRetrun` draws $K \times G$ *placebo* treatment groups of size $N$, where $K$ is the number of random draws at each event date $g$ and $G$ and $N$ denote the number of *unique* event dates, respectively the actual number firms $i$ in the treatment group. The cumulative abnormal return effect is significant at the 10\%, 5\%, or 1\% level if the actual estimated treatment effect $\widehat{\phi}$ lies outside of the interval that contains the $\left[5,95\right]$, $\left[2.5,97.5\right]$, or $\left[0.5,99.5\right]$ percentiles of the *placebo* treatment effects $\widehat{\phi}_{\text{placebo}}$.
