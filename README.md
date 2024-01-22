
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
#> pm  0.41339822 0.19085926 0.039320937 0.7874755
```

``` r
summary(fit, type="OR", conf.int=0.95)
#>             est        se    CI.low    CI.up
#> OR_sde 1.830818 0.5221452 0.8074320 2.854204
#> OR_sie 1.410378 0.2130413 0.9928253 1.827932
#> OR_oe  2.582146 0.6202299 1.3665177 3.797774
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
#> pm  0.40486811 0.18981409 0.032839333 0.7768969
```

``` r
summary(fitSL, type="OR")
#>             est        se    CI.low    CI.up
#> OR_sde 1.833796 0.5171016 0.8202953 2.847296
#> OR_sie 1.396220 0.2090870 0.9864173 1.806023
#> OR_oe  2.560383 0.6118490 1.3611810 3.759585
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
#> pm  0.40697324 0.18946752 0.035623726 0.7783228
```

``` r
summary(fitSL2, type="OR")
#>             est        se    CI.low    CI.up
#> OR_sde 1.830842 0.5159832 0.8195330 2.842150
#> OR_sie 1.399052 0.2089270 0.9895626 1.808542
#> OR_oe  2.561443 0.6116840 1.3625639 3.760321
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
table1[, .(n=mean(n), est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe)), est.pm=mean(est.pm)), by=tar_group]
#>    tar_group    n       est.sde      bias.sde     sd.sde     se.sde cov.sde
#> 1:         1  400  0.0011568832  0.0011675242 0.07301953 0.06670194   0.924
#> 2:         3  400  0.0744694423 -0.0019184837 0.08114126 0.07308826   0.908
#> 3:         5  400  0.0823141914 -0.0022656746 0.07881146 0.07451760   0.915
#> 4:         8  400 -0.0002295364 -0.0002110494 0.07124763 0.06367252   0.919
#> 5:         2 4000 -0.0002590403 -0.0002599653 0.02229865 0.02217322   0.945
#> 6:         6 4000  0.0766324371  0.0002319491 0.02583902 0.02478594   0.944
#> 7:         7 4000  0.0858370618  0.0012594558 0.02513973 0.02560213   0.956
#> 8:         4 4000 -0.0002038614 -0.0001967194 0.02169651 0.02184883   0.943
#>         est.sie      bias.sie     sd.sie     se.sie cov.sie       est.oe
#> 1:  0.046665945 -5.841492e-03 0.05752552 0.04863800   0.899  0.047822828
#> 2:  0.059313102 -4.213797e-03 0.06694654 0.05494333   0.884  0.133782545
#> 3: -0.004865074 -6.550397e-04 0.06370281 0.05700712   0.898  0.077449118
#> 4: -0.005046788 -1.978592e-03 0.05505391 0.04578347   0.879 -0.005276324
#> 5:  0.052234378 -4.556606e-04 0.01785291 0.01698496   0.933  0.051975338
#> 6:  0.062478101 -1.035404e-03 0.02099953 0.01951755   0.943  0.139110538
#> 7: -0.004639951 -4.833688e-04 0.02088282 0.02064219   0.945  0.081197111
#> 8: -0.003104280 -2.308892e-05 0.01752025 0.01685544   0.933 -0.003308141
#>          bias.oe      sd.oe      se.oe cov.oe     est.pm
#> 1: -0.0046739679 0.04706333 0.04794442  0.959  1.4652489
#> 2: -0.0061322805 0.04960323 0.05012726  0.952  0.4214187
#> 3: -0.0029207143 0.04687635 0.04659996  0.943  0.8453446
#> 4: -0.0021896411 0.04239586 0.04282797  0.952  1.1477740
#> 5: -0.0007156259 0.01527452 0.01515538  0.947 -0.1139832
#> 6: -0.0008034551 0.01613308 0.01590818  0.942  0.5455132
#> 7:  0.0007760870 0.01451190 0.01473282  0.959  1.0605876
#> 8: -0.0002198083 0.01349094 0.01359353  0.954 -1.9187879
```

``` r
table1[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))), by=tar_group]
#>    tar_group    n  est.sde    bias.sde    sd.sde    se.sde cov.sde   est.sie
#> 1:         1  400 1.084999 0.085058637 0.5668006 0.4751024   0.885 1.4315645
#> 2:         3  400 1.637291 0.115380606 0.8135160 0.6860396   0.890 1.4347843
#> 3:         5  400 1.811524 0.136104395 0.9305888 0.8082223   0.889 1.0616911
#> 4:         8  400 1.089352 0.089478100 0.6871560 0.5333424   0.876 1.0924682
#> 5:         2 4000 1.004717 0.004706026 0.1407433 0.1386059   0.939 1.3592423
#> 6:         6 4000 1.535707 0.013693719 0.2097943 0.2000887   0.931 1.3501997
#> 7:         7 4000 1.702838 0.027396189 0.2432116 0.2471112   0.954 0.9842119
#> 8:         4 4000 1.005688 0.005732754 0.1573304 0.1583410   0.937 0.9905519
#>       bias.sie    sd.sie    se.sie cov.sie    est.oe     bias.oe      sd.oe
#> 1: 0.083932392 0.4765855 0.3964575   0.893 1.3747786 0.027233205 0.39296684
#> 2: 0.088306181 0.4694264 0.3805097   0.883 2.0984516 0.049497714 0.59681575
#> 3: 0.083890672 0.3620927 0.3254496   0.914 1.7083572 0.070141916 0.54957564
#> 4: 0.114391069 0.4253841 0.3605970   0.904 1.0134048 0.035457352 0.33600504
#> 5: 0.010610780 0.1488658 0.1394654   0.937 1.3497718 0.001135517 0.11844717
#> 6: 0.004289089 0.1382954 0.1287657   0.928 2.0514024 0.002947922 0.17544654
#> 7: 0.006136634 0.1072128 0.1064993   0.946 1.6561956 0.017497463 0.15139075
#> 8: 0.012568985 0.1240265 0.1184905   0.944 0.9811734 0.003241230 0.09558595
#>         se.oe cov.oe
#> 1: 0.38662848  0.926
#> 2: 0.57388639  0.940
#> 3: 0.51448571  0.940
#> 4: 0.32434512  0.937
#> 5: 0.11754273  0.948
#> 6: 0.17294340  0.944
#> 7: 0.15319720  0.957
#> 8: 0.09656312  0.950
```

### Table 2 <a name="table2"></a>

``` r
table2 <- targets::tar_read(table2)
setDT(table2)
table2[, .(est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=.(n, mis)]
```

``` r
table2[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))), by=tar_group]
```

### Table D1 <a name="tableD1"></a>

``` r
tableD1 <- targets::tar_read(tableD1)
setDT(tableD1)
tableD1[, .(est.psi00=mean(est.psi00), bias.psi00 = mean(est.psi00-psi00.true), sd.psi00=sd(est.psi00), se.psi00=mean(se.psi00), cov.psi00 = mean((psi00.true < est.psi00 + qnorm(0.975)*se.psi00) & (psi00.true > est.psi00 - qnorm(0.975)*se.psi00)), est.psi11=mean(est.psi11), bias.psi11 = mean(est.psi11-psi11.true), sd.psi11=sd(est.psi11), se.psi11=mean(se.psi11), cov.psi11 = mean((psi11.true < est.psi11 + qnorm(0.975)*se.psi11) & (psi11.true > est.psi11 - qnorm(0.975)*se.psi11)), est.psi10=mean(est.psi10), bias.psi10 = mean(est.psi10-psi10.true), sd.psi10=sd(est.psi10), se.psi10=mean(se.psi10), cov.psi10 = mean((psi10.true < est.psi10 + qnorm(0.975)*se.psi10) & (psi10.true > est.psi10 - qnorm(0.975)*se.psi10))), by=.(n, n_bins)]
```

``` r
tableD1[, .(est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=.(n, n_bins)]
```

``` r
tableD1[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))), by=tar_group]
```

### Table D2 <a name="tableD2"></a>

``` r
tableD2 <- targets::tar_read(tableD2)
setDT(tableD2)
tableD2[, .(n=mean(n),est.sde=mean(est.sde, na.rm=T), bias.sde = mean(est.sde-sde.true, na.rm=T), sd.sde=sd(est.sde, na.rm=T), se.sde=mean(se.sde, na.rm=T), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde), na.rm=T), est.sie=mean(est.sie, na.rm=T), bias.sie = mean(est.sie-sie.true, na.rm=T), sd.sie=sd(est.sie, na.rm=T), se.sie=mean(se.sie, na.rm=T), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie), na.rm=T), est.oe=mean(est.oe, na.rm=T), bias.oe = mean(est.oe-oe.true, na.rm=T), sd.oe=sd(est.oe, na.rm=T), se.oe=mean(se.oe, na.rm=T), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe), na.rm=T)), by=tar_group]
```

``` r
tableD2[, .(n=mean(n), est.sde=mean(est.ORsde, na.rm=T), bias.sde = mean(est.ORsde-sde.OR.true, na.rm=T), sd.sde=sd(est.ORsde, na.rm=T), se.sde=mean(se.ORsde, na.rm=T), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde), na.rm=T), est.sie=mean(est.ORsie, na.rm=T), bias.sie = mean(est.ORsie-sie.OR.true, na.rm=T), sd.sie=sd(est.ORsie, na.rm=T), se.sie=mean(se.ORsie, na.rm=T), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie), na.rm=T), est.oe=mean(est.ORoe, na.rm=T), bias.oe = mean(est.ORoe-oe.OR.true, na.rm=T), sd.oe=sd(est.ORoe, na.rm=T), se.oe=mean(se.ORoe, na.rm=T), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe), na.rm=T)), by=tar_group]
```
