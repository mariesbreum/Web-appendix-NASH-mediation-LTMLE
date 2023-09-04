# -----------------------------------------------------------------------------
# Run TMLE
# -----------------------------------------------------------------------------
#

# libraries
library(sl3)
library(data.table)

# load code
setwd("C:\\Users\\VXMB\\OneDrive - Novo Nordisk\\Documents\\NASH-mediation")
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}

# true values
data00 <- simulateData(10^6, a =0, a.prime=0)
data11 <- simulateData(10^6, a =1, a.prime=1)
data10 <- simulateData(10^6, a =1, a.prime=0)
te.0 <- data11[, mean(Y)] - data00[, mean(Y)]
sde.0 <-data10[, mean(Y)] - data00[, mean(Y)]
sie.0 <-data11[, mean(Y)] - data10[, mean(Y)]
psi00.0 <-data00[, mean(Y)]
psi10.0 <-data10[, mean(Y)]
psi11.0 <-data11[, mean(Y)]
c(sde.0, sie.0, te.0)


data00 <- simulateData(10^6, a =0, a.prime=0, betaL1.A=0, betaL2.A=0, betaY.A=0, betaY.AM=0)
data11 <- simulateData(10^6, a =1, a.prime=1, betaL1.A=0, betaL2.A=0, betaY.A=0, betaY.AM=0)
data10 <- simulateData(10^6, a =1, a.prime=0, betaL1.A=0, betaL2.A=0, betaY.A=0, betaY.AM=0)

data00 <- simulateData(10^6, a =0, a.prime=0)
data11 <- simulateData(10^6, a =1, a.prime=1)
data10 <- simulateData(10^6, a =1, a.prime=0)


# G formulas
Cmodel = list("C1 ~ A + L01", "C2 ~ A + L1 + M1")
Mmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
gmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
RYmodel = "RY ~ A + M2 + L2"
Ymodel = "Y ~ A + M2 + A:M2 + L2 + L02"
QLmodel = list("QL1 ~ L01 + L02 + A", "QL2 ~ L01 + L02 + A + L1 + M1")




res <- list()
trueval<- list()
for(i in 395:1000){
  data <- simulateData(4000);
  fit_bin40 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01"), Anode = "A", Cnodes = c("C1", "C2"),
                       Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                       Cmodel= list("C1 ~ A", "C2 ~ A + M1"), 
                       Mmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"),
                       gmodel=list("M1 ~ A + L1", "M2 ~ M1 + A + L2"), 
                       RYmodel= "RY ~ A + M2 + L2", 
                       Ymodel="Y ~ A + M2 + L2", 
                       QLmodel= list("QL1 ~ L01 + A", "QL2 ~ L01 + L1 + A + M1"),
                       a1 = 1, a0 = 0, n_bins = 40);
  trueVal <- theTruth(n=10^6,coefM1 = fit_bin40$fitg[[1]]$coefficients, coefM2 = fit_bin40$fitg[[2]]$coefficients,
                      sdM1 =sd(fit_bin40$fitg[[1]]$residuals) , sdM2 = sd(fit_bin40$fitg[[2]]$residuals), pi=fit_bin40$pi)
  res[[i]] <- fit_bin40$est
  trueval[[i]] <-trueVal
  print(i)
}


Res <- matrix(unlist(res), ncol = 18, byrow = TRUE)
Truth <- matrix(unlist(trueval), ncol = 9, byrow = TRUE)

#Truth2 <- theTruth(n=10^6,coefM1 =c(0.5, 0.85, 0.75), coefM2 = c(0.5, 0.9, 1.0, -0.2),
#                    sdM1 =1 , sdM2 = 1, pi=0.5)
#sde.0 <- Truth2$sde.true
#sie.0 <- Truth2$sie.true
#te.0 <- Truth2$oe.true
#gsde.0 <- Truth2$gsde.true


# results sde
mean(na.omit(Res[,1]))
mean(na.omit(Res[,1])-Truth[,4])
sd(Res[,1]); mean(sqrt(Res[,2]))
prop.table(table(Truth[,4] > Res[,1] - qnorm(0.975)*sqrt(Res[,2]) & Truth[,4] < Res[,1] + qnorm(0.975)*sqrt(Res[,2]))) 
prop.table(table(sde.0 > Res[,1] - qnorm(0.975)*sqrt(Res[,2]) & sde.0 < Res[,1] + qnorm(0.975)*sqrt(Res[,2]))) 

# results sie
mean(na.omit(Res[,3]))
mean(na.omit(Res[,3])-Truth[,5])
sd(Res[,3]); mean(sqrt(Res[, 4])) 
prop.table(table(Truth[,5] > Res[,3] - qnorm(0.975)*sqrt(Res[,4]) & Truth[,5] < Res[,3] + qnorm(0.975)*sqrt(Res[,4])))
prop.table(table(sie.0 > Res[,3] - qnorm(0.975)*sqrt(Res[,4]) & sie.0 < Res[,3] + qnorm(0.975)*sqrt(Res[,4])))

#result te
mean(na.omit(Res[,5]))
mean(na.omit(Res[,5])-Truth[,6])
sd(Res[,5]); mean(sqrt(Res[, 6])) 
prop.table(table(Truth[,6] > Res[,5] - qnorm(0.975)*sqrt(Res[, 6]) & Truth[,6] < Res[,5] + qnorm(0.975)*sqrt(Res[, 6])))
prop.table(table(te.0> Res[,5] - qnorm(0.975)*sqrt(Res[, 6]) & te.0 < Res[,5] + qnorm(0.975)*sqrt(Res[, 6])))

#result gsde
mean(na.omit(Res[,7]))
mean(na.omit(Res[,7])-Truth[,7])
sd(Res[,7]); mean(sqrt(Res[, 8])) 
prop.table(table(Truth[,7] > Res[,7] - qnorm(0.975)*sqrt(Res[, 8]) & Truth[,7] < Res[,7] + qnorm(0.975)*sqrt(Res[, 8])))
prop.table(table(gsde.0 > Res[,7] - qnorm(0.975)*sqrt(Res[, 8]) & gsde.0 < Res[,7] + qnorm(0.975)*sqrt(Res[, 8])))



# psi11
mean(na.omit(Res[,11]))
mean(na.omit(Res[,11])-Truth[,1])
sd(Res[,11]); mean(sqrt(Res[,12]))
prop.table(table(Truth[,1] > Res[,11] - qnorm(0.975)*sqrt(Res[,12]) & Truth[,1] < Res[,11] + qnorm(0.975)*sqrt(Res[,12])))


#psi01
mean(na.omit(Res[,13]))
mean(na.omit(Res[,13])-Truth[,2]) 
sd(Res[,13]); mean(sqrt(Res[,14]))
prop.table(table(Truth[,2] > Res[,13] - qnorm(0.975)*sqrt(Res[,14]) & Truth[,2] < Res[,13] + qnorm(0.975)*sqrt(Res[,14])))

#psi00
mean(na.omit(Res[,15]))
mean(na.omit(Res[,15])-Truth[,3])
sd(Res[,15]); mean(sqrt(Res[, 16]))
prop.table(table(Truth[,3] > Res[,15] - qnorm(0.975)*sqrt(Res[,16]) & Truth[,3] < Res[,15] + qnorm(0.975)*sqrt(Res[,16])))



