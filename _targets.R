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
    name = table1,
    command = {
      setting <- list(betaL1.A=betaL1.A, betaM1.A=0.75, 
                      betaL2.A=betaL2.A,betaM2.A=1.00, betaY.A=betaY.A,
                      betaY.M=betaY.M, betaY.AL=0.00, betaY.L=-0.15, alphaY=-1)
      data <- do.call(simulateData, c(list(n=n), setting));
      fit <- do.call(fitLTMLE, c(nodes, list(data=data, Ymodel=Ymodel, QLmodel=QLmodel,
                                             Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                             Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                             gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                             RYmodel= "RY ~ A + M2 + L2",
                                             a1=1, a0=0)));
      trueVal <- do.call(theTruth, c(setting, list(n=10^6, fitg = fit$fitg)))
      cbind(fit$est.all, trueVal)
    },
    values = values_table1,
    reps = 1000
  ),
  tar_map_rep(
    name = table2,
    command = {
      data <- do.call(simulateData, c(list(n=n), setting_ii));
      fit_mis1 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis1));
      fit_mis2 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis2));
      fit_mis3 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis3));
      trueVal <- do.call(theTruth, c(setting_ii, list(n=10^6, fitg = fit_mis1$fitg)))
      rbind(cbind(mis="mis1", fit_mis1$est.all, trueVal),
            cbind(mis="mis2", fit_mis2$est.all, trueVal),
            cbind(mis="mis3", fit_mis3$est.all, trueVal))
    },
    values = tibble::tibble(n=c(400, 4000)),
    reps = 1000
  ),
  tar_map_rep(
    name = tableD1,
    command = {
      data <- do.call(simulateData, c(list(n=n), setting_ii));
      fit10 <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0, n_bins=10)));
      fit20 <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0, n_bins=20)));
      fit40 <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0, n_bins=40)));
      fit80 <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0, n_bins=80)));
      fit160 <- do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0, n_bins=160)));
      trueVal <- do.call(theTruth, c(setting_ii, list(n=10^6,fitg = fit10$fitg)))
      rbind(cbind(n_bins=10, fit10$est.all, trueVal),
            cbind(n_bins=20, fit20$est.all, trueVal),
            cbind(n_bins=40, fit40$est.all, trueVal),
            cbind(n_bins=80, fit80$est.all, trueVal),
            cbind(n_bins=160, fit160$est.all, trueVal))
    },
    values = tibble::tibble(n=c(400, 4000)),
    reps = 1000
  ),
  tar_map_rep(
    name = tableD2,
    command = {
      setting <- list(betaL1.A=0.75, betaM1.A=betaM1.A, 
                      betaL2.A=1.00 ,betaM2.A=betaM2.A, betaY.A=0.75,
                      betaY.M=betaY.M, betaY.AL=0, betaY.L=-0.15, alphaY=-1);
      data <- do.call(simulateData, c(list(n=n), setting));
      fit <- tryCatch({do.call(fitLTMLE, c(nodes, formula_correct, list(data=data, a1=1, a0=0)))}, error=function(e){NULL});
      trueVal <- do.call(theTruth, c(setting, list(n=10^6,fitg = fit$fitg)));
      if(!is.null(fit)){
        cbind(fit$est.all, trueVal)
      }
      else{
        cbind(rep(NA,22), trueVal)
      }
    },
    values = values_tableD2,
    reps = 1000
  ),
  tar_map_rep(
    name = table2new,
    command = {
      data <- do.call(simulateData, c(list(n=n), setting_ii));
      fit_mis4 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis4));
      fit_mis5 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis5));
      fit_mis6 <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula_mis6));
      trueVal <- do.call(theTruth, c(setting_ii, list(n=10^6, fitg = fit_mis4$fitg)))
      rbind(cbind(mis="mis4", fit_mis4$est.all, trueVal),
            cbind(mis="mis5", fit_mis5$est.all, trueVal),
            cbind(mis="mis6", fit_mis6$est.all, trueVal))
    },
    values = tibble::tibble(n=c(400, 4000)),
    reps = 1000
  ),
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


