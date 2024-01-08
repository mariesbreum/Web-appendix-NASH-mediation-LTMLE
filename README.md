
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimation of data-dependent (in)direct effects with a repeatedly measured continuous mediator and missing outcome data

## Introduction <a name="introduction"></a>

Here we provide the R-code to reproduce the simulation study presented
in our manuscript.

## Overview

- [Preparation](#preparation)
- [Example](#example)
  1.  [Simple example](#simplexample)
  2.  [Data adaptive estimation](#dataadaptive)
- [Simulation study](#simulations)
  1.  [Table 1](#table1)
  2.  [Table 2](#table2)
  3.  [Table D1](#tableD1)
  4.  [Table D2](#tableD2)

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
set from the data generating mechanism described in the manuscript.

``` r
set.seed(67394)
data <- simulateData(n=500)
```

The estimates are computed using the <tt>`fitLTMLE`</tt> function

``` r
fit <- fitLTMLE(data, L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
                Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                RYmodel = "RY ~ A + M2 + L2", 
                Ymodel = "Y ~ A + M2 + L2", 
                QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                a1 = 1, a0 = 0)
```

``` r
summary(fit, type="diff", conf.int=0.95)
#>            est         se      CI.low     CI.up
#> sde 0.10629133 0.05169714 0.004966801 0.2076158
#> sie 0.07490711 0.03098414 0.014179306 0.1356349
#> oe  0.18119844 0.04411706 0.094730579 0.2676663
```

``` r
summary(fit, type="OR", conf.int=0.95)
#>             est        se   CI.low    CI.up
#> OR_sde 1.830818 0.4093687 1.028470 2.633166
#> OR_sie 1.410378 0.1497081 1.116956 1.703801
#> OR_oe  2.582146 0.3959921 1.806016 3.358276
```

``` r
summary(fit, type="prop", conf.int=0.95)
#>                est        se     CI.low     CI.up
#> prop_sde 0.5866018 0.1908593 0.21252450 0.9606791
#> prop_sie 0.4133982 0.1908593 0.03932094 0.7874755
```

### Data adaptive estimation <a name="dataadaptive"></a>

Nuisance parameters can be modeled with any machine learning algorithm
supported by the [<tt>`sl3`</tt>](https://github.com/tlverse/sl3) R
package. To illustrate this we define a Super Learner

``` r
lrn_stack <- Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                       Lrnr_gam$new())
lrn_sl <- Lrnr_sl$new(learners = lrn_stack)
```

The estimates are computed as follows

``` r
fitSL <- fitLTMLE(data, L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
                  Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                  gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                  RYmodel = "RY ~ A + M2 + L2", 
                  Ymodel = "Y ~ A + M2 + L2", 
                  QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                  a1 = 1, a0 = 0, Ylearner = lrn_sl, RYlearner = lrn_sl, Clearner = lrn_sl)
```

``` r
summary(fitSL, type="diff")
#>            est         se      CI.low     CI.up
#> sde 0.10702494 0.05129647 0.006485698 0.2075642
#> sie 0.07280904 0.03089198 0.012261869 0.1333562
#> oe  0.17983398 0.04407563 0.093447327 0.2662206
```

``` r
summary(fitSL, type="OR")
#>             est        se   CI.low    CI.up
#> OR_sde 1.833796 0.4049833 1.040043 2.627549
#> OR_sie 1.396220 0.1492736 1.103649 1.688791
#> OR_oe  2.560383 0.3936565 1.788830 3.331936
```

``` r
summary(fitSL, type="prop")
#>                est        se     CI.low     CI.up
#> prop_sde 0.5951319 0.1898141 0.22310312 0.9671607
#> prop_sie 0.4048681 0.1898141 0.03283933 0.7768969
```

We can also specify a super learner for the update steps

``` r
stack <- sl3::Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                        Lrnr_gam$new(), Lrnr_caret$new(algorithm  = "glmStepAIC", trace=F))
corP_screen <- sl3::Lrnr_screener_correlation$new(type = "threshold", pvalue_threshold = 0.05, min_screen = 1)
lrn_QL <- sl3::Lrnr_sl$new(learners = Stack$new(stack, Pipeline$new(corP_screen, stack)))
```

The estimates are computed as follows

``` r
fitSL2 <- fitLTMLE(data, L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
                  Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                  gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                  RYmodel = "RY ~ A + M2 + L2", 
                  Ymodel = "Y ~ A + M2 + L2", 
                  QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                  a1 = 1, a0 = 0, Ylearner = lrn_sl, RYlearner = lrn_sl, Clearner = lrn_sl,
                  QLlearner=lrn_QL)
```

``` r
summary(fitSL2, type="diff")
#>            est         se      CI.low     CI.up
#> sde 0.10675197 0.05124666 0.006310358 0.2071936
#> sie 0.07326009 0.03079037 0.012912068 0.1336081
#> oe  0.18001205 0.04402465 0.093725331 0.2662988
```

``` r
summary(fitSL2, type="OR")
#>             est        se   CI.low    CI.up
#> OR_sde 1.830842 0.4038036 1.039401 2.622282
#> OR_sie 1.399052 0.1488589 1.107294 1.690810
#> OR_oe  2.561443 0.3925751 1.792009 3.330876
```

``` r
summary(fitSL2, type="prop")
#>                est        se     CI.low     CI.up
#> prop_sde 0.5930268 0.1894675 0.22167725 0.9643763
#> prop_sie 0.4069732 0.1894675 0.03562373 0.7783228
```

## Simulation study <a name="simulations"></a>

Our simulations studies are organized with the help of the R package
[<tt>`targets`</tt>](https://books.ropensci.org/targets/). The
simulation set-up is defined in the master file ./\_targets.R. The
results can be assessed by the function <tt>`tar_read()`</tt> as shown
below.

### Table 1 <a name="table1"></a>

``` r
x <- tar_read(table1)
```

### Table 2 <a name="table2"></a>

``` r
x <- tar_read(table2)
```

### Table D1 <a name="tableD1"></a>

``` r
x <- tar_read(tableD1)
```

### Table D2 <a name="tableD2"></a>

``` r
x <- tar_read(tableD2)
```
