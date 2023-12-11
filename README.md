
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimation of data-dependent (in)direct effects with a repeatedly measured continuous mediator and missing outcome data

## Introduction

Here we provide the R-code to reproduce the simulation study presented
in our manuscript.

## Preparation

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

## Example

``` r
data <- simulateData(n=500)
```

``` r
fit <-fitLTMLE(data, t = c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
               Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
               Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
               Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
               gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
               RYmodel = "RY ~ A + M2 + L2", 
               Ymodel = "Y ~ A + M2 + L2", 
               QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
               a1 = 1, a0 = 0, n_bins = 40)
fit$est
#>      par        est          var
#> 1:   sde 0.07525518 0.0043907842
#> 2:   sie 0.04010699 0.0025264039
#> 3:    oe 0.11536217 0.0019474829
#> 4: psi11 0.31227915 0.0010734833
#> 5: psi10 0.27217216 0.0035248918
#> 6: psi00 0.19691697 0.0008841459
```

Nuisance parameters can be modeled with any machine learning algorithm
supported by the [<tt>`sl3`</tt>](https://github.com/tlverse/sl3) R
package

``` r
lrn_stack <- Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                       Lrnr_gam$new())
lrn_sl <- Lrnr_sl$new(learners = lrn_stack)
```

``` r
fitSL <-fitLTMLE(data, t = c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                 Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
                 Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                 gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                 RYmodel = "RY ~ A + M2 + L2", 
                 Ymodel = "Y ~ A + M2 + L2", 
                 QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                 a1 = 1, a0 = 0, n_bins = 40,
                 Ylearner = lrn_sl, RYlearner = lrn_sl, Clearner = lrn_sl)
fitSL$est
#>      par        est         var
#> 1:   sde 0.07643358 0.004296557
#> 2:   sie 0.03952267 0.002475387
#> 3:    oe 0.11595626 0.001969123
#> 4: psi11 0.31302324 0.001080487
#> 5: psi10 0.27350056 0.003414826
#> 6: psi00 0.19706698 0.000898441
```

## Simulation study

Our simulations studies are organized with the help of the targets
package (see <https://books.ropensci.org/targets/>). The simulation
set-up is defined in the master file ./\_targets.R. The results can be
assessed by the function tar_read() as shown below.

## References
