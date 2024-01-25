
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
- [Simulation studies](#simulations)
  1.  [Simulation study 1](#sim1)
  2.  [Simulation study 2](#sim2)
  3.  [Simulation study 3](#sim3)
  4.  [Appendix D](#simD)

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
setting_ii <- list(betaL1.A=0.75, betaM1.A=0.75, betaL2.A=1.00,betaM2.A=1.00,
                        betaY.A=0.75, betaY.M=0.20, betaY.AL=0.0, betaY.L=-0.15, alphaY=-1)
data <- do.call(simulateData, c(list(n=500), setting_ii))
```

The estimates are computed using the <tt>`fitLTMLE`</tt> function

``` r
fit <- fitLTMLE(data = data, L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
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
package. To illustrate this we define a Super Learner for binary
outcomes

``` r
lrn_stack <- Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                       Lrnr_gam$new())
lrn_sl <- Lrnr_sl$new(learners = lrn_stack)
```

We use this learner to estimate the nuisance models $Q_Y$, $p_{R_Y}$ and
$p_C$ by specifying the <tt>`Ylearner`</tt>, <tt>`RYlearner`</tt> and
<tt>`Clearner`</tt> arguments as follows

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
#> sie 0.07280904 0.03089198 0.012261862 0.1333562
#> oe  0.17983397 0.04407563 0.093447322 0.2662206
#> pm  0.40486808 0.18981409 0.032839298 0.7768969
```

``` r
summary(fitSL, type="OR")
#>             est        se    CI.low    CI.up
#> OR_sde 1.833796 0.5171016 0.8202953 2.847296
#> OR_sie 1.396220 0.2090870 0.9864173 1.806023
#> OR_oe  2.560383 0.6118490 1.3611809 3.759585
```

We can also construct a super learner for the update steps $Q_L$ as
follows

``` r
stack <- sl3::Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                        Lrnr_gam$new(), Lrnr_caret$new(algorithm  = "glmStepAIC", trace=F))
corP_screen <- sl3::Lrnr_screener_correlation$new(type = "threshold", pvalue_threshold = 0.05, min_screen = 1)
lrn_QL <- sl3::Lrnr_sl$new(learners = Stack$new(stack, Pipeline$new(corP_screen, stack)),
                           metalearner = Lrnr_nnls$new())
```

We use this learner to estimate $Q_L$ by specifying the
<tt>`QLlearner`</tt> argument as follows

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
#> sde 0.10671801 0.05124585 0.006277989 0.2071580
#> sie 0.07327493 0.03078984 0.012927953 0.1336219
#> oe  0.17999294 0.04402486 0.093705809 0.2662801
#> pm  0.40709890 0.18949643 0.035692720 0.7785051
```

``` r
summary(fitSL2, type="OR")
#>             est        se    CI.low    CI.up
#> OR_sde 1.830458 0.5158508 0.8194091 2.841507
#> OR_sie 1.399150 0.2089427 0.9896303 1.808671
#> OR_oe  2.561086 0.6115621 1.3624464 3.759726
```

## Simulation studies <a name="simulations"></a>

Our simulations studies are organized with the help of the R package
[<tt>`targets`</tt>](https://books.ropensci.org/targets/). The
simulation set-up is defined in the master file ./\_targets.R. The
results can be assessed by the function <tt>`tar_read()`</tt> as shown
below.

### Simulation study 1 <a name="sim1"></a>

``` r
sim1 <- targets::tar_read(sim1)
setDT(sim1)
sim1[, .(n=mean(n), est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe)), est.pm=mean(est.pm)), by=tar_group]
#>    tar_group    n       est.sde      bias.sde     sd.sde     se.sde cov.sde
#> 1:         1  400 -0.0028084912 -2.756239e-03 0.07176353 0.06499452   0.907
#> 2:         3  400  0.0707258027 -5.695628e-03 0.07697775 0.07125471   0.900
#> 3:         5  400  0.0799438295 -4.680011e-03 0.07851247 0.07245251   0.907
#> 4:         8  400 -0.0012020504 -1.208636e-03 0.07170636 0.06370477   0.911
#> 5:         2 4000 -0.0006526523 -6.436633e-04 0.02260323 0.02204864   0.940
#> 6:         6 4000  0.0774283622  9.990932e-04 0.02607252 0.02514463   0.939
#> 7:         7 4000  0.0846146616  2.016662e-05 0.02678480 0.02579835   0.937
#> 8:         4 4000  0.0003831100  3.914450e-04 0.02191045 0.02210246   0.948
#>         est.sie      bias.sie     sd.sie     se.sie cov.sie       est.oe
#> 1:  0.048108081 -0.0045458250 0.05720919 0.04680417   0.889  0.045299590
#> 2:  0.061273699 -0.0023518434 0.06074425 0.05295548   0.891  0.131999501
#> 3: -0.001273054  0.0029044503 0.06314491 0.05480656   0.880  0.078670776
#> 4: -0.004428044 -0.0013553715 0.05788869 0.04598714   0.882 -0.005630094
#> 5:  0.052209898 -0.0004137255 0.01735427 0.01685767   0.934  0.051557246
#> 6:  0.062426529 -0.0010130050 0.02085504 0.01991461   0.943  0.139854891
#> 7: -0.005109041 -0.0009417414 0.02215723 0.02087592   0.943  0.079505620
#> 8: -0.002498036  0.0006033600 0.01742739 0.01716306   0.929 -0.002114926
#>          bias.oe      sd.oe      se.oe cov.oe      est.pm
#> 1: -7.302064e-03 0.04800290 0.04789129  0.955 -1.61305893
#> 2: -8.047472e-03 0.04937031 0.05019840  0.953  0.71487131
#> 3: -1.775561e-03 0.04794747 0.04648789  0.946  0.12077552
#> 4: -2.564008e-03 0.04088935 0.04287215  0.966 -0.91479343
#> 5: -1.057389e-03 0.01479312 0.01515523  0.961 -0.42766216
#> 6: -1.391186e-05 0.01570643 0.01592355  0.953  0.45225281
#> 7: -9.215748e-04 0.01488100 0.01473017  0.949 -0.06696996
#> 8:  9.948050e-04 0.01329866 0.01358840  0.960 -0.09219174
```

### Simulation study 2 <a name="sim2"></a>

``` r
sim2 <- targets::tar_read(sim2)
setDT(sim2)
sim2[, .(est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=.(n, mis)]
#>       n  mis    est.sde      bias.sde     sd.sde     se.sde cov.sde    est.sie
#> 1:  400 mis1 0.07699577  0.0005030314 0.07877798 0.07425363   0.932 0.05963838
#> 2:  400 mis2 0.07582094 -0.0006718044 0.07683330 0.07211951   0.926 0.05883371
#> 3:  400 mis3 0.07717170  0.0006789552 0.07796794 0.07329241   0.930 0.05969232
#> 4:  400 mis4 0.07720569  0.0007129516 0.07785650 0.07507021   0.936 0.05958300
#> 5: 4000 mis1 0.07743746  0.0010146667 0.02565957 0.02540659   0.949 0.06293359
#> 6: 4000 mis2 0.07741415  0.0009913572 0.02513301 0.02459698   0.939 0.06263438
#> 7: 4000 mis3 0.07772310  0.0013003068 0.02543732 0.02501532   0.947 0.06292115
#> 8: 4000 mis4 0.07769042  0.0012676231 0.02538990 0.02567527   0.950 0.06292645
#>         bias.sie     sd.sie     se.sie cov.sie    est.oe       bias.oe
#> 1: -0.0037145634 0.06174002 0.05602936   0.920 0.1366341 -0.0032115320
#> 2: -0.0045192269 0.05922222 0.05383063   0.916 0.1346547 -0.0051910313
#> 3: -0.0036606167 0.06098224 0.05531685   0.924 0.1368640 -0.0029816615
#> 4: -0.0037699381 0.06093916 0.05781274   0.936 0.1367887 -0.0030569865
#> 5: -0.0005593613 0.02102113 0.02022459   0.941 0.1403711  0.0004553054
#> 6: -0.0008585713 0.02026536 0.01935823   0.940 0.1400485  0.0001327859
#> 7: -0.0005718014 0.02075706 0.01987958   0.942 0.1406443  0.0007285054
#> 8: -0.0005665026 0.02070802 0.02072904   0.956 0.1406169  0.0007011204
#>         sd.oe      se.oe cov.oe
#> 1: 0.05066540 0.05085163  0.953
#> 2: 0.05064309 0.05065751  0.947
#> 3: 0.05081718 0.05099734  0.952
#> 4: 0.05082219 0.05135052  0.954
#> 5: 0.01592831 0.01606712  0.955
#> 6: 0.01592170 0.01604269  0.957
#> 7: 0.01598803 0.01612430  0.953
#> 8: 0.01599551 0.01624188  0.955
```

### Simulation study 3 <a name="sim3"></a>

``` r
sim3 <- targets::tar_read(sim3)
setDT(sim3)
sim3[, .(n=mean(n),est.sde=mean(est.sde), bias.sde = mean(est.sde-sde.true), sd.sde=sd(est.sde), se.sde=mean(se.sde), cov.sde = mean((sde.true < est.sde + qnorm(0.975)*se.sde) & (sde.true > est.sde - qnorm(0.975)*se.sde)), est.sie=mean(est.sie), bias.sie = mean(est.sie-sie.true), sd.sie=sd(est.sie), se.sie=mean(se.sie), cov.sie = mean((sie.true < est.sie + qnorm(0.975)*se.sie) & (sie.true > est.sie - qnorm(0.975)*se.sie)), est.oe=mean(est.oe), bias.oe = mean(est.oe-oe.true), sd.oe=sd(est.oe), se.oe=mean(se.oe), cov.oe = mean((oe.true < est.oe + qnorm(0.975)*se.oe) & (oe.true > est.oe - qnorm(0.975)*se.oe))), by=tar_group]
#>    tar_group    n    est.sde      bias.sde     sd.sde     se.sde cov.sde
#> 1:         6  400 0.08516863  0.0052935844 0.14227154 0.10283041   0.787
#> 2:         2  400 0.08067313  0.0002850057 0.17062186 0.10587746   0.684
#> 3:         8  400 0.08747061  0.0066390461 0.20562744 0.10654956   0.587
#> 4:         1  400 0.07611733 -0.0050407552 0.21579209 0.08992264   0.455
#> 5:         7 4000 0.08233134  0.0023325896 0.06059190 0.05169999   0.896
#> 6:         5 4000 0.07909872 -0.0012918954 0.08342046 0.06525456   0.849
#> 7:         4 4000 0.07991504 -0.0008959060 0.11275434 0.08154897   0.796
#> 8:         3 4000 0.07531213 -0.0058571940 0.15331977 0.08961361   0.669
#>       est.sie      bias.sie     sd.sie     se.sie cov.sie    est.oe
#> 1: 0.04951909 -0.0089487165 0.13573291 0.09225827   0.771 0.1346877
#> 2: 0.05590927 -0.0065141445 0.16501373 0.09769320   0.656 0.1365824
#> 3: 0.05138591 -0.0132621533 0.19879908 0.10032737   0.560 0.1388565
#> 4: 0.06473416 -0.0003004145 0.20832926 0.08569967   0.443 0.1408515
#> 5: 0.05507883 -0.0030778070 0.05858968 0.04924081   0.899 0.1374102
#> 6: 0.06336086  0.0009974862 0.08341162 0.06341945   0.847 0.1424596
#> 7: 0.06450692 -0.0001667042 0.11204394 0.08017381   0.781 0.1444220
#> 8: 0.06973979  0.0047362406 0.15204911 0.08855191   0.662 0.1450519
#>          bias.oe      sd.oe      se.oe cov.oe
#> 1: -0.0036551321 0.04816405 0.04929308  0.957
#> 2: -0.0062291388 0.04859983 0.04926704  0.953
#> 3: -0.0066231071 0.04926324 0.04923818  0.955
#> 4: -0.0053411697 0.04966860 0.04907911  0.942
#> 5: -0.0007452174 0.01559815 0.01564993  0.958
#> 6: -0.0002944092 0.01570580 0.01562826  0.952
#> 7: -0.0010626102 0.01546349 0.01560062  0.953
#> 8: -0.0011209534 0.01564166 0.01556039  0.945
```

### Appendix D <a name="simD"></a>
