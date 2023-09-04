setwd("/Users/mariebreum/Documents/GitHub/NASH-mediation")# ---------------------------------------------------------------------
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
# Settings
# ---------------------------------------------------------------------
# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multiprocess")
# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# ---------------------------------------------------------------------
# The target flow
# ---------------------------------------------------------------------

list(
  tar_rep(res_n400_bins,
          command={
            data <- simulateData(400);
            fit_bin10 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 10);
            fit_bin20 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 20);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                           Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                           Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                           Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                           gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                           RYmodel= "RY ~ A + M2 + L2", 
                           Ymodel="Y ~ A + M2 + L2", 
                           QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                           a1 = 1, a0 = 0, n_bins = 40);
            fit_bin80 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 80);
            fit_bin160 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 160);
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 =sd(fit_bin40$fitg[[1]]$residuals) , sdM2 = sd(fit_bin40$fitg[[2]]$residuals))
            rbind(cbind(bins=10, fit_bin10$est, trueVal),
                  cbind(bins=20, fit_bin20$est, trueVal),
                  cbind(bins=40, fit_bin40$est, trueVal),
                  cbind(bins=80, fit_bin80$est, trueVal),
                  cbind(bins=160, fit_bin160$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n4000_bins,
          command={
            data <- simulateData(4000);
            fit_bin10 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A + M2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 10);
            fit_bin20 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 20);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 40);
            fit_bin80 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 80);
            fit_bin160 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A + M2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 160);
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 =sd(fit_bin40$fitg[[1]]$residuals) , sdM2 = sd(fit_bin40$fitg[[2]]$residuals))
            rbind(cbind(bins=10, fit_bin10$est, trueVal),
                  cbind(bins=20, fit_bin20$est, trueVal),
                  cbind(bins=40, fit_bin40$est, trueVal),
                  cbind(bins=80, fit_bin80$est, trueVal),
                  cbind(bins=160, fit_bin160$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n4000_mis,
          command={
            data <- simulateData(4000);
            fit_misC <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misRY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ 1", 
                                 Ymodel="Y ~ A + M2  + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misQL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ A", "QL2 ~ A"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misM <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A", "M2 ~ A"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A + M2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 50);
            fit_misYQL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A", 
                                 QLmodel= list("QL1 ~ A", "QL2 ~ A"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misCMRY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                  Mmodel=list("M1 ~ A", "M2 ~ A"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ 1", 
                                  Ymodel="Y ~ A + M2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 50);
            fit_misCMY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                  Mmodel=list("M1 ~ A", "M2 ~ A"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 50);
            trueVal <- theTruth(n=10^6,coefM1 = fit_misC$fitg[[1]]$coefficients, coefM2 = fit_misC$fitg[[2]]$coefficients,
                                sdM1 =sd(fit_misC$fitg[[1]]$residuals) , sdM2 = sd(fit_misC$fitg[[2]]$residuals))
            rbind(cbind(mis="C", fit_misC$est, trueVal),
                  cbind(mis="RY", fit_misRY$est, trueVal),
                  cbind(mis="Y", fit_misY$est, trueVal),
                  cbind(mis="QL", fit_misQL$est, trueVal),
                  cbind(mis="M", fit_misM$est, trueVal),
                  cbind(mis="YQL", fit_misYQL$est, trueVal),
                  cbind(mis="fit_misCMRY", fit_misCMRY$est, trueVal),
                  cbind(mis="fit_misCMY", fit_misCMY$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n400_mis,
          command={
            data <- simulateData(400);
            fit_misC <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                RYmodel= "RY ~ A + M2 + L2", 
                                Ymodel="Y ~ A + M2 + L2", 
                                QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                a1 = 1, a0 = 0, n_bins = 50);
            fit_misRY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ 1", 
                                 Ymodel="Y ~ A + M2  + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                RYmodel= "RY ~ A + M2 + L2", 
                                Ymodel="Y ~ A", 
                                QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                a1 = 1, a0 = 0, n_bins = 50);
            fit_misQL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + L2", 
                                 QLmodel= list("QL1 ~ A", "QL2 ~ A"),
                                 a1 = 1, a0 = 0, n_bins = 50);
            fit_misM <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                Mmodel=list("M1 ~ A", "M2 ~ A"),
                                gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                RYmodel= "RY ~ A + M2 + L2", 
                                Ymodel="Y ~ A + M2 + L2", 
                                QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                a1 = 1, a0 = 0, n_bins = 50);
            fit_misYQL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A", 
                                  QLmodel= list("QL1 ~ A", "QL2 ~ A"),
                                  a1 = 1, a0 = 0, n_bins = 50);
            fit_misCMRY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                   Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                   Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                   Mmodel=list("M1 ~ A", "M2 ~ A"),
                                   gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                   RYmodel= "RY ~ 1", 
                                   Ymodel="Y ~ A + M2 + L2", 
                                   QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                   a1 = 1, a0 = 0, n_bins = 50);
            fit_misCMY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                   Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                   Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                                   Mmodel=list("M1 ~ A", "M2 ~ A"),
                                   gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                   RYmodel= "RY ~ A + M2 + L2", 
                                   Ymodel="Y ~ A", 
                                   QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                   a1 = 1, a0 = 0, n_bins = 50);
            trueVal <- theTruth(n=10^6,coefM1 = fit_misC$fitg[[1]]$coefficients, coefM2 = fit_misC$fitg[[2]]$coefficients,
                                sdM1 =sd(fit_misC$fitg[[1]]$residuals) , sdM2 = sd(fit_misC$fitg[[2]]$residuals))
            rbind(cbind(mis="C", fit_misC$est, trueVal),
                  cbind(mis="RY", fit_misRY$est, trueVal),
                  cbind(mis="Y", fit_misY$est, trueVal),
                  cbind(mis="QL", fit_misQL$est, trueVal),
                  cbind(mis="M", fit_misM$est, trueVal),
                  cbind(mis="YQL", fit_misYQL$est, trueVal),
                  cbind(mis="fit_misCMRY", fit_misCMRY$est, trueVal),
                  cbind(mis="fit_misCMY", fit_misCMY$est, trueVal))
          },
          reps=1000)
)



