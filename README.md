
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimation of interventional effects in clinical trials with a repeatedly measured mediator

## Introduction <a name="introduction"></a>

Here we provide the R-code to reproduce the simulation study presented
in our manuscript.

## Preparation <a name="preparation"></a>

Clone the github repository, then load (and install first if necessary)
the following packages

``` r
library(data.table)
library(sl3)
```

Source all the R functions

``` r
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}
```

## Example <a name="example"></a>

### Simple example <a name="simpleexample"></a>

To illustrate how the implementation works we generate a simulated data
set from the data generating mechanism (ii) described in the manuscript.

``` r
set.seed(67394)
setting_ii <- data.frame(betaL1.A=-0.25, betaM1.A=-3.50, betaL2.A=-0.15,betaM2.A=-0.70, 
                         betaL3.A=-0.10, betaM3.A=-0.25, betaL2.M=0.02, betaL3.M=0.01,
                         betaY.A=0.90, betaY.M=-0.40, betaY.L=-1.00)
data <- do.call(simulateData, c(list(n=500), setting_ii))
```

The estimates are computed using the <tt>`fitLTMLE`</tt> function

``` r
fit <- fitLTMLE(data = data, L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2", "C3"),
                Lnodes = c("L1", "L2", "L3"), Mnodes = c("M1", "M2", "M3"), RYnode = "RY", Ynode = "Y", 
                Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                RYmodel= "RY ~ A + M3 + L3", Ymodel="Y ~ A + M3 + L3", 
                QLmodel= list("QL1 ~ L01 + A", "QL2 ~  A + L1 + M1", "QL3 ~ A + L2 + M2"),
                a1 = 1, a0 = 0)
```

``` r
summary(fit, type="diff", conf.int=0.95)
#>            est         se     CI.low     CI.up
#> ide  0.1829425 0.05208074 0.08086612 0.2850189
#> iie  0.1736942 0.03516035 0.10478118 0.2426072
#> oe   0.3566367 0.02939299 0.29902751 0.4142459
#> gide 0.1386958 0.03698031 0.06621576 0.2111759
```

``` r
summary(fit, type="OR", conf.int=0.95)
#>              est        se   CI.low    CI.up
#> OR_ide  2.136964 0.4929529 1.170794 3.103134
#> OR_iie  2.649498 0.3754110 1.913706 3.385290
#> OR_oe   5.661884 0.9686233 3.763417 7.560350
#> OR_gide 1.931106 0.3698143 1.206283 2.655929
```

### Data adaptive estimation <a name="dataadaptive"></a>

Nuisance parameters can be modeled with any machine learning algorithm
supported by the [<tt>`sl3`</tt>](https://github.com/tlverse/sl3) R
package. To illustrate this we define an ensemble Super Learner

``` r
lrn_stack <- Stack$new(Lrnr_mean$new(), Lrnr_glm_fast$new(), Lrnr_bayesglm$new(), Lrnr_gam$new())
lrn_sl <- Lrnr_sl$new(
  learners = lrn_stack, 
  metalearner = Lrnr_nnls$new()
)
```

We use this learner to estimate the nuisance models $Q_Y$, $p_{R_Y}$,
$Q_L$ and $p_C$ by specifying the <tt>`Ylearner`</tt>,
<tt>`RYlearner`</tt>,<tt>`QLlearner`</tt> and <tt>`Clearner`</tt>
arguments as follows

``` r
fitSL <- fitLTMLE(data, L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2", "C3"),
                Lnodes = c("L1", "L2", "L3"), Mnodes = c("M1", "M2", "M3"), RYnode = "RY", Ynode = "Y", 
                Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                RYmodel= "RY ~ A + M3 + L3", Ymodel="Y ~ A + M3 + L3", 
                QLmodel= list("QL1 ~ L01 + A", "QL2 ~  A + L1 + M1", "QL3 ~ A + L2 + M2"),
                a1 = 1, a0 = 0, Ylearner = lrn_sl, RYlearner = lrn_sl, QLlearner = lrn_sl, 
                Clearner = lrn_sl)
```

``` r
summary(fitSL, type="diff")
#>            est         se     CI.low     CI.up
#> ide  0.1966504 0.05112400 0.09644915 0.2968516
#> iie  0.1706980 0.03403765 0.10398546 0.2374106
#> oe   0.3673484 0.02869136 0.31111435 0.4235824
#> gide 0.1589278 0.03603440 0.08830172 0.2295540
```

``` r
summary(fitSL, type="OR")
#>              est        se   CI.low    CI.up
#> OR_ide  2.276042 0.5251564 1.246755 3.305330
#> OR_iie  2.711702 0.3679149 1.990602 3.432801
#> OR_oe   6.171947 1.0540403 4.106066 8.237828
#> OR_gide 2.142963 0.4114196 1.336595 2.949330
```

## Simulation studies <a name="simulations"></a>

The simulations can be replicated using the script run-sim.R as shown
below

``` r
source("settings/simulation_settings.R")
n <- 400 # sample size
setting <- setting_ii # data generating mechanism
formula <- formula_correct_ii # model formulas 
name <- "sim1-settingII" # name of output 
source("run-sim.R")
```
