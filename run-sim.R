# -----------------------------------------------------------------------------
# Run TMLE
# -----------------------------------------------------------------------------
#

# libraries
library(sl3)
library(data.table)

# load code
setwd("~/GitHub/NASH-mediation")
source("R/helper_fcts.R")
source("R/fitInitial.R")
source("R/fitLTMLE.R")
source("functions/simulateData.R")

# Set-up
n <- 4000
M <- 1000

# true values
data00 <- simSimple(10^6, a =0, a.prime=0)
data11 <- simSimple(10^6, a =1, a.prime=1)
te.0 <- data11[, mean(Y)] - data00[, mean(Y)]

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
for(i in 46:M){
  data <- simSimple(4000)
  fit <- fitLTMLE(data, t=c(1,2), L0nodes = c("L0", "M0"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, RYmodel, Ymodel, QLcov, a = 1, a.prime = 0, n_bins = 20)
  res[[i]] <- fit
  print(i)
}

Res <- matrix(unlist(res), ncol = 6, byrow = TRUE)

# results sde
mean(na.omit(Res[,1]))
sd(Res[,1]); mean(sqrt(Res[,2])); mean(Res[, 5])
prop.table(table(0 > Res[,1] - qnorm(0.975)*sqrt(Res[,2]) & 0 < Res[,1] + qnorm(0.975)*sqrt(Res[,2])))
prop.table(table(0 > Res[,1] - qnorm(0.975)*Res[, 5] & 0 < Res[,1] + qnorm(0.975)*Res[, 5]))

# results sie
mean(na.omit(Res[,3]))
sd(Res[,3]); mean(sqrt(Res[,4])); mean(Res[, 6])
prop.table(table(te.0 > Res[,3] - qnorm(0.975)*sqrt(Res[,4]) & te.0 < Res[,3] + qnorm(0.975)*sqrt(Res[,4])))
prop.table(table(te.0 > Res[,3] - qnorm(0.975)*Res[,6] & te.0 < Res[,3] + qnorm(0.975)*Res[,6]))

