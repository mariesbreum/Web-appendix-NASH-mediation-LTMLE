
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
table1 <- targets::tar_read(table1)
setDT(table1)
table1[, .(est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=tar_group]
#>    tar_group    est.sde     bias.sde      sd.sde     se.sde cov.sde     est.sie
#> 1:         6 0.12648838  0.126411381 0.048569600 0.10622777       1 -0.03254927
#> 2:         2 0.16422114 -0.048175358 0.061621392 0.10335714       1  0.09873746
#> 3:         5 0.24350938  0.006896384 0.054193700 0.11402052       1  0.04544718
#> 4:         8 0.01738624  0.016965236 0.159104233 0.07361353       1 -0.02027316
#> 5:         7 0.01298563  0.012138134 0.022978452 0.02689906       1  0.08642641
#> 6:         4 0.19562409 -0.015749906 0.048869893 0.02755656       1  0.06170574
#> 7:         3 0.23941930  0.002220796 0.005452400 0.02877880       1 -0.01234246
#> 8:         1 0.03884899  0.038283993 0.007424461 0.03656809       1 -0.02064717
#>        bias.sie      sd.sie     se.sie cov.sie       est.oe      bias.oe
#> 1: -0.124327775 0.102823690 0.08672687     0.5  0.093939107  0.002083607
#> 2:  0.027007959 0.030408760 0.08256899     1.0  0.262958601 -0.021167399
#> 3:  0.039049683 0.074906956 0.09585211     1.0  0.288956567  0.045946067
#> 4: -0.025259165 0.047342152 0.04623339     1.0 -0.002886929 -0.008293929
#> 5:  0.002040410 0.018549227 0.01990632     1.0  0.099412044  0.014178544
#> 6: -0.007970762 0.028484713 0.02078916     1.0  0.257329832 -0.023720668
#> 7: -0.017807961 0.009125238 0.02255570     1.0  0.227076835 -0.015587165
#> 8: -0.024831172 0.024081178 0.03129072     1.0  0.018201821  0.013452821
#>          sd.oe      se.oe cov.oe
#> 1: 0.054254090 0.05859015    1.0
#> 2: 0.031212633 0.05419555    1.0
#> 3: 0.020713256 0.05517430    1.0
#> 4: 0.111762081 0.05725664    1.0
#> 5: 0.004429224 0.01805531    1.0
#> 6: 0.020385180 0.01725981    0.5
#> 7: 0.014577638 0.01783480    1.0
#> 8: 0.016656716 0.01803490    1.0
```

### Table 2 <a name="table2"></a>

``` r
x2 <- targets::tar_read(table2)
setDT(x2)
x2[, .(mean(est.sde), sd(est.sde), mean(se.sde), mean(est.sie), sd(est.sie), mean(se.sie), mean(est.oe), sd(est.oe), mean(se.oe)), by=.(n, mis)]
```

### Table D1 <a name="tableD1"></a>

``` r
x3 <- targets::tar_read(tableD1)
setDT(x3)
x3[, .(mean(est.sde), sd(est.sde), mean(se.sde), mean(est.sie), sd(est.sie), mean(se.sie), mean(est.oe), sd(est.oe), mean(se.oe)), by=.(n, n_bins)]
```

### Table D2 <a name="tableD2"></a>

``` r
x4 <- targets::tar_read(tableD2)
setDT(x4)
x4[, .(n, mean(est.sde), sd(est.sde), mean(se.sde), mean(est.sie), sd(est.sie), mean(se.sie), mean(est.oe), sd(est.oe), mean(se.oe)), by=tar_group]
```
