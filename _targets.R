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
  tar_rep(res_n400,
          command={
            data <- simulateData(400);
            fit_bin20 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 20);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                           Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                           Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                           Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                           gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                           RYmodel= "RY ~ A + M2 + L2", 
                           Ymodel="Y ~ A + M2 + A:L2 + L2", 
                           QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                           a1 = 1, a0 = 0, n_bins = 40);
            fit_bin80 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 80);
            fit_bin160 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 160);
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 = data[C1==0, sd(M1)], sdM2 = data[C2==0, sd(M2)])
            rbind(cbind(bins=20, fit_bin20$est, trueVal),
                  cbind(bins=40, fit_bin40$est, trueVal),
                  cbind(bins=80, fit_bin80$est, trueVal),
                  cbind(bins=160, fit_bin160$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n800,
          command={
            data <- simulateData(800);
            fit_bin20 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 20);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 40);
            fit_bin80 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 80);
            fit_bin160 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 160);
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 = data[C1==0, sd(M1)], sdM2 = data[C2==0, sd(M2)])
            rbind(cbind(bins=20, fit_bin20$est, trueVal),
                  cbind(bins=40, fit_bin40$est, trueVal),
                  cbind(bins=80, fit_bin80$est, trueVal),
                  cbind(bins=160, fit_bin160$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n1600,
          command={
            data <- simulateData(1600);
            fit_bin20 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 20);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 40);
            fit_bin80 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 80);
            fit_bin160 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                  Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                  Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                  gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                  RYmodel= "RY ~ A + M2 + L2", 
                                  Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                  QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                  a1 = 1, a0 = 0, n_bins = 160);
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 = data[C1==0, sd(M1)], sdM2 = data[C2==0, sd(M2)])
            rbind(cbind(bins=20, fit_bin20$est, trueVal),
                  cbind(bins=40, fit_bin40$est, trueVal),
                  cbind(bins=80, fit_bin80$est, trueVal),
                  cbind(bins=160, fit_bin160$est, trueVal))
          },
          reps=1000),
  tar_rep(res_n400_Msl,
          command={
            data <- simulateData(400);
            fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                                 Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                                 Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                                 gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                                 RYmodel= "RY ~ A + M2 + L2", 
                                 Ymodel="Y ~ A + M2 + A:L2 + L2", 
                                 QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                 a1 = 1, a0 = 0, n_bins = 40,
                                 Mlearner=sl3::Lrnr_sl$new(learners = Stack$new(
                                   Lrnr_density_semiparametric$new(Lrnr_glm$new()),
                                   Lrnr_density_semiparametric$new(Lrnr_bayesglm$new()),
                                   Lrnr_density_semiparametric$new(Lrnr_glmnetnew()),
                                   Lrnr_density_semiparametric$new(mean_learner=Lrnr_glm$new(), var_learner=Lrnr_glm$new()),
                                 ), metalearner = Lrnr_solnp_density$new()));
            trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                                sdM1 = data[C1==0, sd(M1)], sdM2 = data[C2==0, sd(M2)])
            cbind(bins=40, fit_bin40$est, trueVal)
          },
          reps=1000)
)



