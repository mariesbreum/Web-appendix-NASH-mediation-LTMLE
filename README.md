
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

## Simulation study

Our simulations studies are organized with the help of the targets
package (see <https://books.ropensci.org/targets/>). The simulation
set-up is defined in the master file ./\_targets.R. The results can be
assessed by the function tar_read() as shown below.
