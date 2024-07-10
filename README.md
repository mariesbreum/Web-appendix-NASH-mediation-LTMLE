
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
fitSL <- fitLTMLE(data, L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel = list("C1 ~ A", "C2 ~ A + M1"), 
                  Mmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                  gmodel = list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                  RYmodel = "RY ~ A + M2 + L2", 
                  Ymodel = "Y ~ A + M2 + L2", 
                  QLmodel = list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                  a1 = 1, a0 = 0, Ylearner = lrn_sl, RYlearner = lrn_sl, QLlearner=lrn_sl, Clearner = lrn_sl)
```

``` r
summary(fitSL, type="diff")
#>            est         se      CI.low     CI.up
#> sde 0.10685153 0.05131292 0.006280045 0.2074230
#> sie 0.07339888 0.03080795 0.013016401 0.1337813
#> oe  0.18025040 0.04425957 0.093503233 0.2669976
#> pm  0.40720506 0.18911190 0.036552547 0.7778576
```

``` r
summary(fitSL, type="OR")
#>             est        se    CI.low    CI.up
#> OR_sde 1.832229 0.5175435 0.8178628 2.846596
#> OR_sie 1.399929 0.2090297 0.9902382 1.809619
#> OR_oe  2.564991 0.6157912 1.3580621 3.771919
```

## Simulation studies <a name="simulations"></a>

Our simulations studies are organized with the help of the R package
[<tt>`targets`</tt>](https://books.ropensci.org/targets/). The
simulation set-up is defined in the master file ./\_targets.R. The
results can be assessed by the function <tt>`tar_read()`</tt> as shown
below.
