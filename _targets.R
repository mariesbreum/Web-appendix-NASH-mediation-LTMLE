setwd("~/GitHub/NASH-mediation")
# packages
# ---------------------------------------------------------------------
library(targets)
library(tarchetypes)
thepackages <- c("data.table", 
                 "sl3")
targets::tar_option_set(packages = thepackages)
# ---------------------------------------------------------------------
# R functions
# ---------------------------------------------------------------------
tar_source(files=c("R", "functions"))
# ---------------------------------------------------------------------
# Simulation settings
# ---------------------------------------------------------------------
source("./settings/simulation_settings.R")
# ---------------------------------------------------------------------
# The target flow
# ---------------------------------------------------------------------
list(
  tar_map_rep(
    name = table3,
    command = {
      setting <- list(betaL1.A=0.75, betaM1.A=0.75, 
                      betaL2.A=1.00,betaM2.A=1.00, betaY.A=0.75,
                      betaY.M=0.20, betaY.AL=0.00, betaY.L=-0.15, alphaY=alphaY)
      data <- do.call(simulateData, c(list(n=n), setting));
      fit <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0)));
      trueVal <- do.call(theTruth, c(setting, list(n=10^6, fitg = fit$fitg)))
      cbind(fit$est.all, trueVal)
    },
    values = values_table3,
    reps = 1000
  )
)


