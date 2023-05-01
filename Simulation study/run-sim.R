# -----------------------------------------------------------------------------
# Run TMLE
# -----------------------------------------------------------------------------
#

# libraries
library(sl3)
library(data.table)

# load code
setwd("~/GitHub/NASH-mediation/Simulation study")
source("fcts-helper.R")
source("fcts-initial.R")
source("fcts-tmle.R")
source("simulation-data-simple.R")

# Set-up
n <- 4000
M <- 1000

# true values
data10 <- simSimple(10^6, a =1, a.prime=0)
data00 <- simSimple(10^6, a =0, a.prime=0)
data11 <- simSimple(10^6, a =1, a.prime=1)
sde.0 <- data10[, mean(Y)] - data00[, mean(Y)]
sie.0 <- data11[, mean(Y)] - data10[, mean(Y)]
te.0 <- data11[, mean(Y)] - data00[, mean(Y)]
#c(sde.0, sie.0, te.0)

# G formulas
Cmodel <- list("C1 ~ 1", 
               "C2 ~ 1")
Mmodel <- list("M1 ~ M0 + A + L1", 
               "M2 ~ M1 + A + L2")
RYmodel <- "RY ~ A + M2 + L2"  
Ymodel <- "Y ~ L0 + M1 + L2 + M2"
QLcov <- list("L0 + M0 + A", "L0 + L1 + A + M0 + M1")


# test
data <- simSimple(400)
fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L0", "M0"), Anode = "A", Cnodes = c("C1", "C2"),
            Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
            Cmodel, Mmodel, RYmodel, Ymodel, QLcov, a = 1, a.prime = 0, n_bins = 20)



# run sim
set.seed(2345)
res <- list()
for(i in 345:M){
  data <- simSimple(n)
  fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L0", "M0"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, RYmodel, Ymodel, QLcov, 
                  a = 1, a.prime = 0, n_bins = 50)
  res[[i]] <- fit
  print(i)
}

Res <- matrix(unlist(res), ncol = 4, byrow = TRUE)

# results sde
mean(na.omit(Res[,1])); sd(na.omit(Res[,1])); mean(sqrt(Res[,2]))

# results sie
mean(na.omit(Res[,3])); sd(na.omit(Res[,3])); mean(sqrt(Res[,4]))

save(Res, file="~/GitHub/NASH-mediation/Simulation study/Results/Res_n4000_M350")
