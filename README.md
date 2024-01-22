
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
#>       n  mis    est.sde      bias.sde     sd.sde     se.sde cov.sde    est.sie
#> 1:  400 mis1 0.07778240  0.0012336831 0.07891422 0.07504169   0.930 0.05942479
#> 2:  400 mis2 0.07767081  0.0011220987 0.07806644 0.07259853   0.924 0.05753489
#> 3:  400 mis3 0.07813487  0.0015861495 0.07831533 0.07397661   0.929 0.05935897
#> 4: 4000 mis1 0.07637936 -0.0000349446 0.02616576 0.02535325   0.943 0.06315730
#> 5: 4000 mis2 0.07625156 -0.0001627451 0.02547638 0.02454728   0.936 0.06296330
#> 6: 4000 mis3 0.07670314  0.0002888273 0.02592053 0.02495568   0.944 0.06312405
#>         bias.sie     sd.sie     se.sie cov.sie    est.oe       bias.oe
#> 1: -0.0040673261 0.06208190 0.05706352   0.929 0.1372072 -0.0028336430
#> 2: -0.0059572323 0.06034169 0.05448251   0.924 0.1352057 -0.0048351337
#> 3: -0.0041331504 0.06136479 0.05624047   0.929 0.1374938 -0.0025470009
#> 4: -0.0003992605 0.02110614 0.02018097   0.941 0.1395367 -0.0004342051
#> 5: -0.0005932558 0.02028826 0.01932954   0.934 0.1392149 -0.0007560009
#> 6: -0.0004325095 0.02085562 0.01983137   0.938 0.1398272 -0.0001436822
#>         sd.oe      se.oe cov.oe
#> 1: 0.05110581 0.05077325  0.947
#> 2: 0.05107035 0.05059581  0.949
#> 3: 0.05123908 0.05093031  0.949
#> 4: 0.01563728 0.01606388  0.960
#> 5: 0.01564499 0.01603915  0.960
#> 6: 0.01570944 0.01612189  0.959
```

``` r
table2[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))),  by=.(n, mis)]
#>       n  mis    n  est.sde   bias.sde    sd.sde    se.sde cov.sde  est.sie
#> 1:  400 mis1  400 1.660989 0.13804587 0.7589402 0.6822139   0.910 1.418856
#> 2:  400 mis2  400 1.657807 0.13486440 0.7510201 0.6641337   0.904 1.398826
#> 3:  400 mis3  400 1.659375 0.13643210 0.7486960 0.6692089   0.910 1.413930
#> 4: 4000 mis1 4000 1.533559 0.01142253 0.2100204 0.2033034   0.946 1.354669
#> 5: 4000 mis2 4000 1.532134 0.00999803 0.2052127 0.1974234   0.938 1.352660
#> 6: 4000 mis3 4000 1.533314 0.01117806 0.2072851 0.1994652   0.941 1.352899
#>       bias.sie    sd.sie    se.sie cov.sie   est.oe     bias.oe     sd.oe
#> 1: 0.072745791 0.4434444 0.4090593   0.918 2.136339 0.086613368 0.6425756
#> 2: 0.052715523 0.4091070 0.3663847   0.908 2.115212 0.065485692 0.6327978
#> 3: 0.067819644 0.4349758 0.3993807   0.916 2.133875 0.084148775 0.6403361
#> 4: 0.008510873 0.1421175 0.1352350   0.943 2.054158 0.005157708 0.1707933
#> 5: 0.006501470 0.1351687 0.1282658   0.936 2.051078 0.002077667 0.1705240
#> 6: 0.006739989 0.1397016 0.1321025   0.941 2.051973 0.002972268 0.1707210
#>        se.oe cov.oe
#> 1: 0.5903807  0.940
#> 2: 0.5815996  0.934
#> 3: 0.5880308  0.942
#> 4: 0.1746725  0.951
#> 5: 0.1738629  0.949
#> 6: 0.1740967  0.953
```

``` r
table3 <- targets::tar_read(table3)
setDT(table3)
table3[, .(n=mean(n),alphaY=mean(alphaY),est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=tar_group]
#>    tar_group    n alphaY    est.sde      bias.sde     sd.sde     se.sde cov.sde
#> 1:         3  400   -3.0 0.01802501  0.0006377577 0.03616706 0.03048308   0.895
#> 2:         5  400   -2.5 0.02515270 -0.0017797351 0.04389001 0.03866611   0.907
#> 3:         6  400   -2.0 0.04466069  0.0043368990 0.05818997 0.05103492   0.926
#> 4:         2  400   -1.5 0.05734545 -0.0001438955 0.07007235 0.06275989   0.895
#> 5:         4 4000   -3.0 0.01793496  0.0005526767 0.01223554 0.01119676   0.930
#> 6:         8 4000   -2.5 0.02704639  0.0001334155 0.01426910 0.01404310   0.938
#> 7:         7 4000   -2.0 0.03917403 -0.0011724002 0.01757468 0.01731478   0.935
#> 8:         1 4000   -1.5 0.05598875 -0.0014235673 0.02196513 0.02116548   0.930
#>       est.sie      bias.sie     sd.sie      se.sie cov.sie     est.oe
#> 1: 0.01440752 -0.0026812534 0.02919041 0.022572720   0.930 0.03243253
#> 2: 0.02437940 -0.0015009408 0.03348658 0.028373302   0.915 0.04953210
#> 3: 0.03215483 -0.0052439368 0.04724128 0.038327935   0.902 0.07681552
#> 4: 0.04466305 -0.0058449723 0.05455021 0.047300627   0.907 0.10200850
#> 5: 0.01669959 -0.0003567820 0.01024367 0.008812842   0.913 0.03463456
#> 6: 0.02508549 -0.0007631167 0.01174295 0.011060759   0.926 0.05213188
#> 7: 0.03677598 -0.0005178896 0.01398986 0.013610817   0.934 0.07595001
#> 8: 0.05094826  0.0002207250 0.01782848 0.016705260   0.922 0.10693701
#>          bias.oe       sd.oe       se.oe cov.oe
#> 1: -0.0020434958 0.025284707 0.024923675  0.941
#> 2: -0.0032806759 0.030527858 0.030895037  0.946
#> 3: -0.0009070378 0.036871010 0.037502310  0.956
#> 4: -0.0059888678 0.044508815 0.044097408  0.944
#> 5:  0.0001958947 0.008041475 0.007965279  0.950
#> 6: -0.0006297012 0.009616286 0.009837540  0.959
#> 7: -0.0016902898 0.011402162 0.011929689  0.956
#> 8: -0.0012028423 0.013543817 0.014023734  0.956
```

``` r
table3[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))), by=tar_group]
#>    tar_group    n      est.sde      bias.sde       sd.sde       se.sde cov.sde
#> 1:         3  400 1.014827e+08  1.014827e+08 1.350179e+09 5.052827e+07   0.818
#> 2:         5  400 1.794324e+00  2.609756e-01 1.417482e+00 1.139588e+00   0.849
#> 3:         6  400 1.790228e+00  2.598049e-01 1.078742e+00 9.479249e-01   0.893
#> 4:         2  400 1.685393e+00  1.587302e-01 9.183302e-01 7.788264e-01   0.872
#> 5:         4 4000 1.594551e+00  5.888415e-02 4.470900e-01 4.069613e-01   0.922
#> 6:         8 4000 1.558928e+00  2.600264e-02 3.266079e-01 3.211841e-01   0.929
#> 7:         7 4000 1.526048e+00 -4.506545e-03 2.643242e-01 2.594013e-01   0.927
#> 8:         1 4000 1.524755e+00 -1.377065e-03 2.326991e-01 2.239402e-01   0.925
#>     est.sie   bias.sie    sd.sie    se.sie cov.sie       est.oe       bias.oe
#> 1: 1.741615 0.38543749 1.0089453 0.6361929   0.829 1.721288e+08  1.721288e+08
#> 2: 1.598568 0.24388439 0.7049902 0.5264938   0.873 2.355134e+00  2.783001e-01
#> 3: 1.486290 0.13440053 0.5862696 0.4607417   0.875 2.272708e+00  2.040619e-01
#> 4: 1.441700 0.09475674 0.4984781 0.4047720   0.897 2.138565e+00  8.254224e-02
#> 5: 1.393227 0.03823320 0.2590093 0.2276644   0.923 2.142312e+00  6.155214e-02
#> 6: 1.370943 0.01722285 0.2053557 0.1908323   0.920 2.089612e+00  1.452092e-02
#> 7: 1.364903 0.01464297 0.1678236 0.1617623   0.931 2.050123e+00 -1.648097e-02
#> 8: 1.366044 0.01785025 0.1547381 0.1436676   0.929 2.055410e+00 -2.076326e-03
#>           sd.oe        se.oe cov.oe
#> 1: 1.976472e+09 5.317683e+07  0.900
#> 2: 1.347537e+00 1.186478e+00  0.914
#> 3: 9.104876e-01 8.874135e-01  0.944
#> 4: 7.316538e-01 6.837893e-01  0.925
#> 5: 4.012148e-01 3.843449e-01  0.946
#> 6: 2.967011e-01 2.986627e-01  0.957
#> 7: 2.316117e-01 2.377186e-01  0.947
#> 8: 1.975618e-01 1.997144e-01  0.953
```

### Table D1 <a name="tableD1"></a>

``` r
tableD1 <- targets::tar_read(tableD1)
setDT(tableD1)
tableD1[, .(est.psi00=mean(est.psi00), bias.psi00 = mean(est.psi00-psi00.true), sd.psi00=sd(est.psi00), se.psi00=mean(se.psi00), cov.psi00 = mean((psi00.true < est.psi00 + qnorm(0.975)*se.psi00) & (psi00.true > est.psi00 - qnorm(0.975)*se.psi00)), est.psi11=mean(est.psi11), bias.psi11 = mean(est.psi11-psi11.true), sd.psi11=sd(est.psi11), se.psi11=mean(se.psi11), cov.psi11 = mean((psi11.true < est.psi11 + qnorm(0.975)*se.psi11) & (psi11.true > est.psi11 - qnorm(0.975)*se.psi11)), est.psi10=mean(est.psi10), bias.psi10 = mean(est.psi10-psi10.true), sd.psi10=sd(est.psi10), se.psi10=mean(se.psi10), cov.psi10 = mean((psi10.true < est.psi10 + qnorm(0.975)*se.psi10) & (psi10.true > est.psi10 - qnorm(0.975)*se.psi10))), by=.(n, n_bins)]
#>        n n_bins est.psi00    bias.psi00   sd.psi00   se.psi00 cov.psi00
#>  1:  400     10 0.1981154 -0.0052624921 0.03332003 0.03347899     0.940
#>  2:  400     20 0.2011946 -0.0021832847 0.03373836 0.03349099     0.945
#>  3:  400     40 0.2017809 -0.0015969114 0.03383161 0.03349482     0.943
#>  4:  400     80 0.2019288 -0.0014490982 0.03385755 0.03349632     0.943
#>  5:  400    160 0.2019748 -0.0014030939 0.03386635 0.03349698     0.943
#>  6: 4000     10 0.2020061 -0.0011413662 0.01059653 0.01062379     0.944
#>  7: 4000     20 0.2028610 -0.0002864276 0.01060728 0.01062534     0.945
#>  8: 4000     40 0.2029674 -0.0001800557 0.01060923 0.01062563     0.946
#>  9: 4000     80 0.2029862 -0.0001612573 0.01060918 0.01062572     0.946
#> 10: 4000    160 0.2029900 -0.0001574378 0.01060897 0.01062575     0.946
#>     est.psi11   bias.psi11   sd.psi11   se.psi11 cov.psi11 est.psi10
#>  1: 0.3093475 -0.033679437 0.03634449 0.03736491     0.845 0.2717939
#>  2: 0.3299444 -0.013082543 0.03591948 0.03739647     0.939 0.2734943
#>  3: 0.3357324 -0.007294634 0.03598056 0.03741219     0.947 0.2735349
#>  4: 0.3377984 -0.005228612 0.03603358 0.03741915     0.950 0.2734296
#>  5: 0.3386606 -0.004366434 0.03606153 0.03742235     0.949 0.2733522
#>  6: 0.3309291 -0.012125412 0.01309278 0.01185561     0.798 0.2794126
#>  7: 0.3396253 -0.003429187 0.01226627 0.01186343     0.928 0.2799197
#>  8: 0.3412810 -0.001773586 0.01220539 0.01186584     0.936 0.2799272
#>  9: 0.3417594 -0.001295124 0.01219630 0.01186667     0.939 0.2799024
#> 10: 0.3419383 -0.001116188 0.01219401 0.01186700     0.940 0.2798849
#>        bias.psi10   sd.psi10   se.psi10 cov.psi10
#>  1: -0.0079550398 0.06516579 0.06114727     0.889
#>  2: -0.0062545847 0.06519133 0.06114916     0.895
#>  3: -0.0062139624 0.06508258 0.06115258     0.896
#>  4: -0.0063192573 0.06500736 0.06115472     0.896
#>  5: -0.0063966718 0.06496539 0.06115589     0.896
#>  6: -0.0001843018 0.02331146 0.02267278     0.941
#>  7:  0.0003227636 0.02333719 0.02267346     0.946
#>  8:  0.0003302442 0.02333586 0.02267373     0.946
#>  9:  0.0003054417 0.02333349 0.02267386     0.946
#> 10:  0.0002879579 0.02333200 0.02267392     0.945
```

``` r
tableD1[, .(est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=.(n, n_bins)]
#>        n n_bins    est.sde      bias.sde     sd.sde     se.sde cov.sde
#>  1:  400     10 0.07367850 -0.0026925477 0.07247986 0.07031554   0.935
#>  2:  400     20 0.07229975 -0.0040713001 0.07291971 0.07031972   0.933
#>  3:  400     40 0.07175400 -0.0046170511 0.07291430 0.07032345   0.932
#>  4:  400     80 0.07150089 -0.0048701591 0.07287542 0.07032558   0.932
#>  5:  400    160 0.07137747 -0.0049935779 0.07284873 0.07032671   0.931
#>  6: 4000     10 0.07740656  0.0009570644 0.02548242 0.02510822   0.953
#>  7: 4000     20 0.07705869  0.0006091912 0.02552339 0.02510922   0.950
#>  8: 4000     40 0.07695980  0.0005102999 0.02552446 0.02510952   0.950
#>  9: 4000     80 0.07691620  0.0004666990 0.02552278 0.02510965   0.950
#> 10: 4000    160 0.07689489  0.0004453957 0.02552156 0.02510971   0.950
#>        est.sie     bias.sie     sd.sie     se.sie cov.sie    est.oe
#>  1: 0.03755369 -0.025724397 0.05771513 0.05182339   0.925 0.1112322
#>  2: 0.05645013 -0.006827958 0.05797308 0.05180039   0.911 0.1287499
#>  3: 0.06219741 -0.001080671 0.05808262 0.05179885   0.911 0.1339514
#>  4: 0.06436873  0.001090646 0.05811705 0.05179976   0.903 0.1358696
#>  5: 0.06530832  0.002030238 0.05812781 0.05180054   0.902 0.1366858
#>  6: 0.05151648 -0.011941110 0.02152984 0.01988505   0.929 0.1289230
#>  7: 0.05970564 -0.003751950 0.02095514 0.01988302   0.940 0.1367643
#>  8: 0.06135376 -0.002103830 0.02088480 0.01988307   0.940 0.1383136
#>  9: 0.06185703 -0.001600565 0.02086666 0.01988318   0.938 0.1387732
#> 10: 0.06205345 -0.001404146 0.02085981 0.01988324   0.937 0.1389483
#>           bias.oe      sd.oe      se.oe cov.oe
#>  1: -0.0284169452 0.04907343 0.05019174  0.917
#>  2: -0.0108992585 0.04967339 0.05020949  0.959
#>  3: -0.0056977224 0.04987709 0.05021916  0.959
#>  4: -0.0037795134 0.04995794 0.05022359  0.957
#>  5: -0.0029633404 0.04999306 0.05022565  0.959
#>  6: -0.0109840457 0.01652512 0.01590895  0.888
#>  7: -0.0031427590 0.01620411 0.01591366  0.940
#>  8: -0.0015935299 0.01619991 0.01591519  0.947
#>  9: -0.0011338662 0.01620375 0.01591573  0.947
#> 10: -0.0009587506 0.01620579 0.01591594  0.948
```

``` r
tableD1[, .(n=mean(n), est.sde=mean(est.ORsde), bias.sde = mean(est.ORsde-sde.OR.true), sd.sde=sd(est.ORsde), se.sde=mean(se.ORsde), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde)), est.sie=mean(est.ORsie), bias.sie = mean(est.ORsie-sie.OR.true), sd.sie=sd(est.ORsie), se.sie=mean(se.ORsie), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie)), est.oe=mean(est.ORoe), bias.oe = mean(est.ORoe-oe.OR.true), sd.oe=sd(est.ORoe), se.oe=mean(se.ORoe), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe))), by=.(n, n_bins)]
#>        n n_bins    n  est.sde   bias.sde    sd.sde    se.sde cov.sde  est.sie
#>  1:  400     10  400 1.616114 0.09461091 0.6825416 0.6349573   0.907 1.274928
#>  2:  400     20  400 1.599687 0.07818428 0.6765532 0.6249001   0.901 1.390745
#>  3:  400     40  400 1.594216 0.07271328 0.6737228 0.6223388   0.901 1.427578
#>  4:  400     80  400 1.591860 0.07035756 0.6723034 0.6214107   0.899 1.441756
#>  5:  400    160  400 1.590748 0.06924582 0.6715881 0.6210118   0.898 1.447944
#>  6: 4000     10 4000 1.542887 0.02048245 0.2078696 0.2034049   0.948 1.285548
#>  7: 4000     20 4000 1.538604 0.01619907 0.2072924 0.2025196   0.947 1.333136
#>  8: 4000     40 4000 1.537647 0.01524262 0.2071533 0.2023730   0.947 1.342925
#>  9: 4000     80 4000 1.537278 0.01487340 0.2070965 0.2023297   0.947 1.345944
#> 10: 4000    160 4000 1.537107 0.01470297 0.2070696 0.2023125   0.947 1.347130
#>          bias.sie    sd.sie    se.sie cov.sie   est.oe      bias.oe     sd.oe
#>  1: -0.0700128105 0.3704842 0.3251043   0.872 1.898766 -0.147229331 0.5420037
#>  2:  0.0458048337 0.4044782 0.3527419   0.903 2.048775  0.002779504 0.5819752
#>  3:  0.0826377646 0.4160601 0.3619981   0.906 2.095590  0.049595104 0.5949910
#>  4:  0.0968151687 0.4206043 0.3656849   0.908 2.113269  0.067273972 0.6000108
#>  5:  0.1030032055 0.4225935 0.3673271   0.909 2.120883  0.074888414 0.6021933
#>  6: -0.0600063447 0.1333007 0.1249504   0.896 1.962782 -0.085644834 0.1731273
#>  7: -0.0124186585 0.1348955 0.1294392   0.940 2.030081 -0.018345999 0.1765141
#>  8: -0.0026289528 0.1354856 0.1303927   0.943 2.043774 -0.004652759 0.1776532
#>  9:  0.0003902653 0.1356898 0.1306953   0.942 2.047895 -0.000531685 0.1780363
#> 10:  0.0015755404 0.1357722 0.1308165   0.943 2.049479  0.001051793 0.1781882
#>         se.oe cov.oe
#>  1: 0.5305637  0.872
#>  2: 0.5611540  0.922
#>  3: 0.5714135  0.936
#>  4: 0.5754297  0.938
#>  5: 0.5771936  0.940
#>  6: 0.1668711  0.892
#>  7: 0.1714318  0.939
#>  8: 0.1724003  0.945
#>  9: 0.1726994  0.946
#> 10: 0.1728161  0.946
```

### Table D2 <a name="tableD2"></a>

``` r
tableD2 <- targets::tar_read(tableD2)
setDT(tableD2)
tableD2[, .(n=mean(n),est.sde=mean(est.sde, na.rm=T), bias.sde = mean(est.sde-sde.true, na.rm=T), sd.sde=sd(est.sde, na.rm=T), se.sde=mean(se.sde, na.rm=T), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde), na.rm=T), est.sie=mean(est.sie, na.rm=T), bias.sie = mean(est.sie-sie.true, na.rm=T), sd.sie=sd(est.sie, na.rm=T), se.sie=mean(se.sie, na.rm=T), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie), na.rm=T), est.oe=mean(est.oe, na.rm=T), bias.oe = mean(est.oe-oe.true, na.rm=T), sd.oe=sd(est.oe, na.rm=T), se.oe=mean(se.oe, na.rm=T), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe), na.rm=T)), by=tar_group]
#>    tar_group    n    est.sde      bias.sde     sd.sde     se.sde   cov.sde
#> 1:         3  400 0.08089951  0.0004964770 0.16704032 0.10760591 0.7080000
#> 2:         2  400 0.07800835 -0.0032906169 0.21630807 0.08978524 0.4680000
#> 3:         8  400 0.08718752  0.0054257072 0.25994223 0.06001517 0.2350000
#> 4:         7  400 0.10795083  0.0257453204 0.29453553 0.04330216 0.1510000
#> 5:         6 4000 0.08130033  0.0009206501 0.08720207 0.06727282 0.8540000
#> 6:         4 4000 0.07126814 -0.0098928461 0.14005140 0.08372481 0.6580000
#> 7:         5 4000 0.08993653  0.0081377688 0.20601436 0.07359409 0.3950000
#> 8:         1 4000 0.07452635 -0.0076461291 0.22916795 0.04315517 0.2006018
#>       est.sie     bias.sie     sd.sie     se.sie   cov.sie    est.oe
#> 1: 0.05563162 -0.006740573 0.15963556 0.09937498 0.7060000 0.1365311
#> 2: 0.05982778 -0.005060470 0.21317534 0.08534198 0.4610000 0.1378361
#> 3: 0.05305809 -0.011750367 0.25829299 0.05950105 0.2390000 0.1402456
#> 4: 0.03446972 -0.030333352 0.29144967 0.04581936 0.1620000 0.1424206
#> 5: 0.06004570 -0.002421511 0.08618676 0.06548983 0.8530000 0.1413460
#> 6: 0.07425244  0.009218519 0.13929637 0.08267101 0.6480000 0.1455206
#> 7: 0.05555419 -0.009155564 0.20521232 0.07301861 0.3990000 0.1454907
#> 8: 0.07139178  0.006488658 0.22883863 0.04319171 0.2056169 0.1459181
#>         bias.oe      sd.oe      se.oe    cov.oe
#> 1: -0.006244096 0.04755765 0.04927257 0.9470000
#> 2: -0.008351087 0.04968036 0.04905890 0.9380000
#> 3: -0.006324660 0.04977257 0.04891934 0.9430000
#> 4: -0.004588031 0.04888645 0.04868333 0.9600000
#> 5: -0.001500861 0.01570742 0.01562010 0.9430000
#> 6: -0.000674327 0.01531799 0.01556323 0.9560000
#> 7: -0.001017795 0.01510452 0.01548409 0.9610000
#> 8: -0.001157471 0.01501497 0.01542613 0.9518556
```

``` r
tableD2[est.ORsde<100, .(n=mean(n), est.sde=mean(est.ORsde, na.rm=T), bias.sde = mean(est.ORsde-sde.OR.true, na.rm=T), sd.sde=sd(est.ORsde, na.rm=T), se.sde=mean(se.ORsde, na.rm=T), cov.sde = mean((sde.OR.true < est.ORsde + qnorm(0.975)*se.ORsde) & (sde.OR.true > est.ORsde - qnorm(0.975)*se.ORsde), na.rm=T), est.sie=mean(est.ORsie, na.rm=T), bias.sie = mean(est.ORsie-sie.OR.true, na.rm=T), sd.sie=sd(est.ORsie, na.rm=T), se.sie=mean(se.ORsie, na.rm=T), cov.sie = mean((sie.OR.true < est.ORsie + qnorm(0.975)*se.ORsie) & (sie.OR.true > est.ORsie - qnorm(0.975)*se.ORsie), na.rm=T), est.oe=mean(est.ORoe, na.rm=T), bias.oe = mean(est.ORoe-oe.OR.true, na.rm=T), sd.oe=sd(est.ORoe, na.rm=T), se.oe=mean(se.ORoe, na.rm=T), cov.oe = mean((oe.OR.true < est.ORoe + qnorm(0.975)*se.ORoe) & (oe.OR.true > est.ORoe - qnorm(0.975)*se.ORoe), na.rm=T)), by=tar_group]
#>    tar_group    n  est.sde  bias.sde     sd.sde   se.sde   cov.sde      est.sie
#> 1:         3  400 2.221438 0.6335719  3.6177871 2.489427 0.7392177 2.497966e+00
#> 2:         2  400 3.016866 1.4129383  6.6515290 3.232224 0.5692771 4.560715e+00
#> 3:         8  400 3.590723 1.9762467  8.6722991 2.068065 0.3530612 1.591553e+01
#> 4:         7  400 5.583371 3.9607749 13.2214259 2.229632 0.2121827 4.039660e+01
#> 5:         6 4000 1.687803 0.1001047  0.8463537 0.652217 0.8488488 1.519958e+00
#> 6:         4 4000 1.909805 0.3070164  2.5475251 1.294399 0.6710000 2.201868e+00
#> 7:         5 4000 2.509621 0.8949119  4.5099134 2.230725 0.4569402 1.708080e+08
#> 8:         1 4000 2.948482 1.3262029  7.9744067 1.862219 0.2474542 3.551252e+08
#>        bias.sie       sd.sie       se.sie   cov.sie   est.oe       bias.oe
#> 1: 1.149446e+00 6.638723e+00 9.919580e+00 0.7933801 2.183841  0.0426928687
#> 2: 3.194583e+00 1.282280e+01 1.069439e+01 0.6255020 2.232751  0.0416525712
#> 3: 1.454803e+01 1.210201e+02 1.876375e+02 0.3948980 2.260997  0.0532631428
#> 4: 3.902811e+01 2.687161e+02 6.256422e+02 0.2426396 2.292904  0.0724470475
#> 5: 1.709909e-01 6.632967e-01 5.343187e-01 0.8758759 2.139159 -0.0025736930
#> 6: 8.348212e-01 2.179558e+00 2.159173e+00 0.7580000 2.194659  0.0035920899
#> 7: 1.708080e+08 5.366198e+09 5.171134e+19 0.5450861 2.207323  0.0005880624
#> 8: 3.551251e+08 7.866993e+09 4.975818e+19 0.3167006 2.219778 -0.0011852724
#>        sd.oe     se.oe    cov.oe
#> 1: 0.5941974 0.6125588 0.9317954
#> 2: 0.6794171 0.6316036 0.9287149
#> 3: 0.6744406 0.6409118 0.9295918
#> 4: 0.6694116 0.6508442 0.9411168
#> 5: 0.1855008 0.1853272 0.9459459
#> 6: 0.1868457 0.1908355 0.9600000
#> 7: 0.1875186 0.1926667 0.9554205
#> 8: 0.1891854 0.1940550 0.9460285
```
