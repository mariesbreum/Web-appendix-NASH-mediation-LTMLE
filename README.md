
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
fit <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
               Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
               Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
               Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
               gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
               RYmodel= "RY ~ A + M2 + L2", 
               Ymodel="Y ~ A + M2 + L2", 
               QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
               a1 = 1, a0 = 0, n_bins = 40)
fit$est
#>      par        est         var
#> 1:   sde 0.11503069 0.007786138
#> 2:   sie 0.00169643 0.005290155
#> 3:    oe 0.11672712 0.002189614
#> 4: psi11 0.35620454 0.001148217
#> 5: psi10 0.35450811 0.006745420
#> 6: psi00 0.23947742 0.001045378
```

Nuisance parameters can be modeled with any machine learning algorithm
supported by sl3 R package

``` r
lrn_stack <- Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                       Lrnr_gam$new())
lrn_sl <- Lrnr_sl$new(learners = lrn_stack)
```

``` r
fitSL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                 RYmodel= "RY ~ A + M2 + L2", 
                 Ymodel="Y ~ A + M2 + L2", 
                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                 a1 = 1, a0 = 0, n_bins = 40,
                 Ylearner=lrn_sl, RYlearner = lrn_sl, Clearner = lrn_sl)
fitSL$est
#>      par        est         var
#> 1:   sde 0.10100678 0.006651031
#> 2:   sie 0.01423091 0.004344599
#> 3:    oe 0.11523769 0.002190961
#> 4: psi11 0.35762046 0.001160378
#> 5: psi10 0.34338955 0.005620386
#> 6: psi00 0.24238276 0.001035007
```

## Simulation study

Our simulations studies are organized with the help of the targets
package (see <https://books.ropensci.org/targets/>). The simulation
set-up is defined in the master file ./\_targets.R. The results can be
assessed by the function tar_read() as shown below.
