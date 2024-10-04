#-------- nodes -------- 
nodes <- list(L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2", "C3"),
              Lnodes = c("L1", "L2", "L3"), Mnodes = c("M1", "M2", "M3"), RYnode = "RY", Ynode = "Y")

#-------- data generating mechanisms -------- 
# no direct effect 
setting_i <- data.frame(betaL1.A=0.00, betaM1.A=-3.50, betaL2.A=0.00,betaM2.A=-0.70, 
                        betaL3.A=0.00, betaM3.A=-0.25, betaL2.M=0.02, betaL3.M=0.01,
                        betaY.A=0.00, betaY.M=-0.40, betaY.L=-1.00)
# both direct and indirect
setting_ii <- data.frame(betaL1.A=-0.25, betaM1.A=-3.50, betaL2.A=-0.15,betaM2.A=-0.70, 
                         betaL3.A=-0.10, betaM3.A=-0.25, betaL2.M=0.02, betaL3.M=0.01,
                         betaY.A=0.90, betaY.M=-0.40, betaY.L=-1.00)
# no indirect effect 
setting_iii <- data.frame(betaL1.A=-0.25, betaM1.A=-3.50, betaL2.A=-0.15,betaM2.A=-0.70, 
                         betaL3.A=-0.10, betaM3.A=-0.25, betaL2.M=0.00, betaL3.M=0.00,
                         betaY.A=0.90, betaY.M=0.00, betaY.L=-1.00)
# no direct or indirect effect 
setting_iv <- data.frame(betaL1.A=0.00, betaM1.A=-3.50, betaL2.A=0.00,betaM2.A=-0.70, 
                         betaL3.A=0.00, betaM3.A=-0.25, betaL2.M=0.00, betaL3.M=0.00,
                         betaY.A=0.00, betaY.M=0.00, betaY.L=-1.00)

#-------- model formulas sim 1 -------- 
formula_correct_i <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                          Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                          gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                          RYmodel= "RY ~ A + M3 + L3", 
                          Ymodel="Y ~ M3 + L3", 
                          QLmodel= list("QL1 ~ L01", "QL2 ~  L1 + M1", 
                                        "QL3 ~ L2 + M2"))
formula_correct_ii <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                           Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                           gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                           RYmodel= "RY ~ A + M3 + L3", 
                           Ymodel="Y ~ A + M3 + L3", 
                           QLmodel= list("QL1 ~ L01 + A", "QL2 ~  A + L1 + M1", 
                                         "QL3 ~ A + L2 + M2"))
formula_correct_iii <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                            Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                            gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                            RYmodel= "RY ~ A + M3 + L3", 
                            Ymodel="Y ~ A + L3", 
                            QLmodel= list("QL1 ~ L01 + A", "QL2 ~  A + L1", 
                                          "QL3 ~ A + L2"))
formula_correct_iv <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                           Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                           gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                           RYmodel= "RY ~ A + M3 + L3", 
                           Ymodel="Y ~ L3", 
                           QLmodel= list("QL1 ~ L01", "QL2 ~ L1 ", 
                                         "QL3 ~ L2 "))

#-------- model formulas sim 2 -------- 
formula_mis_a <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                      Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                      gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                      RYmodel= "RY ~ A + M3 + L3", 
                      Ymodel="Y ~ A", 
                      QLmodel= list("QL1 ~ A", "QL2 ~ A", "QL3 ~ A"))
formula_mis_b <- list(Cmodel= list("C1 ~ A", "C2 ~ A + M1", "C3 ~ A + M2"), 
                      Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                      gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                      RYmodel= "RY ~ 1", 
                      Ymodel="Y ~ A + M3 + L3", 
                      QLmodel= list("QL1 ~ A", "QL2 ~ A", "QL3 ~ A"))
formula_mis_c <- list(Cmodel= list("C1 ~ 1", "C2 ~ 1", "C3 ~ 1"), 
                      Mmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"),
                      gmodel=list("M1 ~ A + L1 + L02", "M2 ~ A + M1 + L2", "M3 ~ A + M2 + L3"), 
                      RYmodel= "RY ~ 1", 
                      Ymodel="Y ~ A", 
                      QLmodel= list("QL1 ~ A", "QL2 ~ A", "QL3 ~ A"))