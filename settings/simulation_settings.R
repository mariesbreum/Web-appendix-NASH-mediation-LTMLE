# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
# Description:
# Settings for the simulation study
#

nodes <- list(L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
              Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y")

# data generating mechanisms:
setting_i <- data.frame(betaL1.A=0, betaM1.A=0.75, betaL2.A=0,betaM2.A=1.00, betaY.A=0,
                  betaY.M=0.20, betaY.AL=0.0, betaY.L=-0.15, alphaY=-1)
setting_ii <- data.frame(betaL1.A=0.75, betaM1.A=0.75, betaL2.A=1.00,betaM2.A=1.00, betaY.A=0.75,
                   betaY.M=0.20, betaY.AL=0.0, betaY.L=-0.15, alphaY=-1)
setting_iii <- data.frame(betaL1.A=0.75, betaM1.A=0.75, betaL2.A=1.00,betaM2.A=1.00, betaY.A=0.75,
                    betaY.M=0, betaY.AL=0.0, betaY.L=-0.15, alphaY=-1)
setting_iv <- data.frame(betaL1.A=0, betaM1.A=0.75, betaL2.A=0,betaM2.A=1.00, betaY.A=0,
                   betaY.M=0, betaY.AL=0.0, betaY.L=-0.15, alphaY=-1)

values_sim1 <- tibble::tibble(n=c(rep(400,4), rep(4000,4)), 
                                betaL1.A=rep(c(0.00, 0.75, 0.75, 0.00),2),
                                betaL2.A=rep(c(0.00, 1.00, 1.00, 0.00),2),
                                betaY.A= rep(c(0.00, 0.75, 0.75, 0.00),2),
                                betaY.M= rep(c(0.20, 0.20, 0.00, 0.00),2),
                                Ymodel = c("Y ~ M2 + L2", "Y ~ A + M2 + L2", "Y ~ A + L2", "Y ~ L2",
                                           "Y ~ M2 + L2", "Y ~ A + M2 + L2", "Y ~ A + L2", "Y ~ L2"),
                                QLmodel =list(list("QL1 ~ L01", "QL2 ~ L01 + L1 + M1"),
                                              list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                              list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                              list("QL1 ~ L01", "QL2 ~ L01 + L1 + M1"),
                                              list("QL1 ~ L01", "QL2 ~ L01 + L1 + M1"),
                                              list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                              list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                                              list("QL1 ~ L01", "QL2 ~ L01 + L1 + M1")))

# model formulas:
formula_correct <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                        Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                        gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                        RYmodel= "RY ~ A + M2 + L2", 
                        Ymodel="Y ~ A + M2 + L2", 
                        QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"))
formula_mis1 <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                     Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                     gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                     RYmodel= "RY ~ A + M2 + L2", 
                     Ymodel="Y ~ A", 
                     QLmodel= list("QL1 ~ A", "QL2 ~ A"))
formula_mis2 <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                     Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                     gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                     RYmodel= "RY ~ 1", 
                     Ymodel="Y ~ A + M2 + L2", 
                     QLmodel= list("QL1 ~ A", "QL2 ~ A"))
formula_mis3 <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                     Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                     gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                     RYmodel= "RY ~ 1", 
                     Ymodel="Y ~ A", 
                     QLmodel= list("QL1 ~ A", "QL2 ~ A"))
formula_mis4 <- list(Cmodel= list("C1 ~ 1", "C2 ~ 1"), 
                     Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                     gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                     RYmodel= "RY ~ 1", 
                     Ymodel="Y ~ A", 
                     QLmodel= list("QL1 ~ A", "QL2 ~ A"))



values_sim3 <- tibble::tibble(n=c(rep(400,4), rep(4000,4)), 
                                 betaM1.A = rep(c(1.25, 1.50, 1.75, 2.00), 2),
                                 betaM2.A = rep(c(1.50, 1.75, 2.00, 2.25), 2),
                                 betaY.M = rep(c(0.125, 0.115, 0.105, 0.095), 2))