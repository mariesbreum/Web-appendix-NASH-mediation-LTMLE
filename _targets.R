setwd("C:\\Users\\mcn456\\Documents\\GitHub\\NASH-mediation")
# ---------------------------------------------------------------------
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

sim.fct <- function(n, 
                    Cmodel, 
                    Mmodel, 
                    gmodel, 
                    RYmodel, 
                    Ymodel, 
                    QLmodel, 
                    n_bins,
                    betaL1.A,
                    betaM1.A, 
                    betaL2.A, 
                    betaM2.A, 
                    betaY.A, 
                    betaY.M, 
                    betaY.AM, 
                    betaY.L){
  
  data <- simulateData(n, betaL1.A,betaM1.A, betaL2.A, betaM2.A, betaY.A, betaY.M, betaY.AM, betaY.L)
  fit <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                 Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins)
  data.frame(fit)
}


list(
  tar_map_rep(test,
              values=values=tibble::tibble(n=c(400,800, 4000)),
              command = sim.fct(n, betaL1.A=0.75, betaM1.A=1.75, betaL2.A=0.50, betaM2.A=1.50, 
                                betaY.A=0.50, betaY.M=0.75, betaY.AM=0.20, betaY.L=0.01,
                                Cmodel = list("C1 ~ A + L01", "C2 ~ A + L1 + M1"),
                                Mmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02"),
                                gmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02"),
                                RYmodel = "RY ~ A + M2 + L2",  
                                Ymodel = "Y ~ A + M2 + A:M2 + L2 + L02",
                                QLmodel = list("QL1 ~ L01 + L02 + A", "QL2 ~ L01 + L02 + A + L1 + M1")),
              reps=500,
              batches=2)
  
  #tar_map_rep(table1,
  #            values=data.frame(n = c(rep(400, 3), rep(800, 3)),
  #                              betaL1.A= rep(c(0.75, 0.75, 0.00), 2),
  #                              betaM1.A= rep(c(1.75, 0.00, 1.75), 2), 
  #                              betaL2.A= rep(c(0.50, 0.50, 0.00), 2), 
  #                              betaM2.A= rep(c(1.50, 0.00, 1.50), 2), 
  #                              betaY.A = rep(c(0.50, 0.50, 0.00), 2), 
  #                              betaY.M = rep(c(0.75, 0.00, 0.75), 2), 
  #                              betaY.AM= rep(c(0.20, 0.00, 0.00), 2),
  #                              betaY.L = rep(c(0.01, 0.01, 0.00), 2)),
  #            command = sim.fct(n, betaL1.A, betaM1.A, betaL2.A, betaM2.A, 
  #                              betaY.A, betaY.M, betaY.AM, betaY.L,
  #                              Cmodel = list("C1 ~ A + L01", "C2 ~ A + L1 + M1"),
  #                              Mmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02"),
  #                              gmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02"),
  #                              RYmodel = "RY ~ A + M2 + L2",  
  #                              Ymodel = "Y ~ A + M2 + A:M2 + L2 + L02",
  #                              QLmodel = list("QL1 ~ L01 + L02 + A", "QL2 ~ L01 + L02 + A + L1 + M1")),
  #            reps=500,
  #            batches=2),
  #tar_map_rep(table2,
  #            values=data.frame(n = c(400, 800)),
  #            command = {
  #              data <- simulateData(n, betaL1.A=0.75, betaM1.A=1.75, betaL2.A=0.50, betaM2.A=1.50, 
  #                                   betaY.A=0.50, betaY.M=0.75, betaY.AM=0.20, betaY.L=0.01);
  #              fit_mis <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                                  Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              fit_misM <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                             Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                             Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              fit_misQL <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                              Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                              Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              fit_misC <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                                  Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              fit_misRY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                                  Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              fit_misY <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
  #                                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
  #                                  Cmodel, Mmodel, RYmodel, Ymodel, QLmodel,a = 1, a.prime = 0, n_bins = n_bins);
  #              data.frame(rbind())
  #              },
  #            reps=500,
  #            batches=2)
)


 
  

