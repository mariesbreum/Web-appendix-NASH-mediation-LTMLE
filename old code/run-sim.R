# -----------------------------------------------------------------------------
# Run TMLE
# -----------------------------------------------------------------------------
#

# libraries
library(sl3)
library(data.table)

# load code
setwd("~/GitHub/NASH-mediation")
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}


# true values
data00 <- simulateData(10^6, a =0, a.prime=0)
data11 <- simulateData(10^6, a =1, a.prime=1)
data10 <- simulateData(10^6, a =1, a.prime=0)
te.0 <- data11[, mean(Y)] - data00[, mean(Y)]
sde.0 <-data10[, mean(Y)] - data00[, mean(Y)]
sie.0 <-data11[, mean(Y)] - data10[, mean(Y)]


# G formulas
Cmodel = list("C1 ~ A + L01", "C2 ~ A + L1 + M1")
Mmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
gmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
RYmodel = "RY ~ A + M2 + L2"
Ymodel = "Y ~ A + M2 + A:M2 + L2 + L02"
QLmodel = list("QL1 ~ L01 + L02 + A", "QL2 ~ L01 + L02 + A + L1 + M1")


# test
Mlearner=glearner=NULL
data <- simulateData(800)
fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
            Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
            Cmodel, Mmodel, Mmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 30,
            glearner=glearner, Mlearner=Mlearner)

hose_hal_lrnr <- Lrnr_density_semiparametric$new(
  mean_learner = Lrnr_glm$new()
)
hese_rf_glm_lrnr <- Lrnr_density_semiparametric$new(
  mean_learner = Lrnr_glm$new(),
  var_learner = Lrnr_glm$new()
)

# SL for the conditional treatment density
sl_dens_lrnr <- Lrnr_sl$new(
  learners = list(hose_hal_lrnr, hese_rf_glm_lrnr),
  metalearner = Lrnr_solnp_density$new()
)


# run sim
set.seed(2345)
res <- list()
res2 <- list()

for(i in 26:500){
  data <- simulateData(400)
  fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, Mmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 20, 
                  Mlearner=sl_dens_lrnr, glearner=sl_dens_lrnr)
  fit2 <- fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, Mmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 20, 
                  Mlearner=NULL, glearner=NULL)
  res[[i]] <- fit
  res2[[i]] <- fit2
  print(i)
}

Res <- matrix(unlist(res2), ncol = 9, byrow = TRUE)

# results sde
mean(na.omit(Res[,1]))
sd(Res[,1]); mean(sqrt(Res[,2])); mean(Res[, 3])
prop.table(table(sde.0 > Res[,1] - qnorm(0.975)*sqrt(Res[,2]) & sde.0 < Res[,1] + qnorm(0.975)*sqrt(Res[,2])))
prop.table(table(sde.0 > Res[,1] - qnorm(0.975)*Res[, 3] & sde.0 < Res[,1] + qnorm(0.975)*Res[, 3]))

# results sie
mean(na.omit(Res[,4]))
sd(Res[,4]); mean(sqrt(Res[,5])); mean(Res[, 6])
prop.table(table(sie.0 > Res[,4] - qnorm(0.975)*sqrt(Res[,5]) & sie.0 < Res[,4] + qnorm(0.975)*sqrt(Res[,5])))
prop.table(table(sie.0 > Res[,4] - qnorm(0.975)*Res[,6] & sie.0 < Res[,4] + qnorm(0.975)*Res[,6]))


mean(na.omit(Res[,7]))
sd(Res[,7]); mean(sqrt(Res[,8])); mean(Res[, 9])
prop.table(table(te.0 > Res[,7] - qnorm(0.975)*sqrt(Res[,8]) & te.0 < Res[,7] + qnorm(0.975)*sqrt(Res[,8])))
prop.table(table(te.0 > Res[,7] - qnorm(0.975)*Res[,9] & te.0 < Res[,7] + qnorm(0.975)*Res[,9]))


set.seed(2345)
res2 <- list()
for(i in 26:500){
  data <- simulateData(4000)
  fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, Mmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 40, 
                  Mlearner=sl_dens_lrnr, glearner=sl_dens_lrnr)
  res2[[i]] <- fit
  print(i)
}