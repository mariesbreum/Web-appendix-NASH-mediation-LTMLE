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

fitM <-list()
fitM[[2]] <- Mlearner$train(make_task(data[C2==0,], Mmodel[[1]]))
fitM[[2]]$predict()

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



#stack <- sl3::Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
#                        Lrnr_gam$new(), Lrnr_caret$new(algorithm  = "glmStepAIC", trace=F))
corP_screen <- sl3::Lrnr_screener_correlation$new(type = "threshold", pvalue_threshold = 0.05, min_screen = 1)
#lrn_QL <- sl3::Lrnr_sl$new(learners = Stack$new(stack, Pipeline$new(corP_screen, stack)))
lrn_QL <- sl3::Pipeline$new(corP_screen, Lrnr_glm_fast$new())

data<- simulateData(10^6, betaL1.A=0, betaL2.A=0, betaY.A=0, betaY.AM=0)
prop.table(table(data$RY))

x <- tar_read(res_n500_noindirect)
mean(x$sie)
sd(x$sie);mean(na.omit(x$siese2))
mean(x$sde+qnorm(0.975)*x$sdese2> sde.0  & sde.0 >x$sde-qnorm(0.975)*x$sdese2)


# G formulas
Cmodel = list("C1 ~ A + L01", "C2 ~ A + L1 + M1")
Mmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
gmodel = list("M1 ~ A + L1 + L02", "M2 ~ M1 + A + L2 + L02")
RYmodel = "RY ~ A + M2 + L2"
Ymodel = "Y ~ A + M2 + A:M2 + L2 + L02"
QLmodel = list("QL1 ~ L01 + L02 + A", "QL2 ~ L01 + L02 + A + L1 + M1")




res <- list()
res2<- list()
for(i in 6:500){
  data <- simulateData(300)
  fit <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                 Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                 Cmodel, Mmodel, gmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 20)
  fit2 <-fitLTMLE(data, t=c(1,2), L0nodes = c("L01", "L02"), Anode = "A", Cnodes = c("C1", "C2"),
                  Lnodes = c("L1", "L2"), Mnodes = c("M1", "M2"), RYnode = "RY", Ynode = "Y", 
                  Cmodel, Mmodel, gmodel, RYmodel, Ymodel, QLmodel, a1 = 1, a0 = 0, n_bins = 20,
                  Mlearner=MlearnerSL, glearner=MlearnerSL)
  res[[i]] <- fit
  res2[[i]] <-fit2
  print(i)
}

Res <- matrix(unlist(res2), ncol = 18, byrow = TRUE)

# results sde
mean(na.omit(Res[,1]))
sd(Res[,1]); mean(sqrt(Res[,2])); mean(Res[, 3]); mean(sqrt(Res[,18]^2+Res[,12]^2)) 
prop.table(table(sde.0 > Res[,1] - qnorm(0.975)*sqrt(Res[,18]^2+Res[,12]^2) & sde.0 < Res[,1] + qnorm(0.975)*sqrt(Res[,18]^2+Res[,12]^2))) 
prop.table(table(sde.0 > Res[,1] - qnorm(0.975)*Res[, 3] & sde.0 < Res[,1] + qnorm(0.975)*Res[, 3]))

# results sie
mean(na.omit(Res[,4]))
sd(Res[,4]); mean(Res[, 6]); mean(sqrt(Res[,12]^2+Res[,15]^2)) 
prop.table(table(sie.0 > Res[,4] - qnorm(0.975)*sqrt(Res[,12]^2+Res[,15]^2) & sie.0 < Res[,4] + qnorm(0.975)*sqrt(Res[,12]^2+Res[,15]^2)))
prop.table(table(sie.0 > Res[,4] - qnorm(0.975)*Res[,6] & sie.0 < Res[,4] + qnorm(0.975)*Res[,6]))

mean(na.omit(Res[,7]))
sd(Res[,7]); mean(Res[, 9]); mean(sqrt(Res[,18]^2+Res[,15]^2)) 
prop.table(table(te.0 > Res[,7] - qnorm(0.975)*sqrt(Res[, 15]^2+Res[, 18]^2) & te.0 < Res[,7] + qnorm(0.975)*sqrt(Res[, 15]^2+Res[, 18]^2)))
prop.table(table(te.0 > Res[,7] - qnorm(0.975)*Res[,9] & te.0 < Res[,7] + qnorm(0.975)*Res[,9]))

mean(na.omit(Res[,10]))
sd(Res[,10]); mean(sqrt(Res[,11])); mean(Res[, 12])
prop.table(table(psi10.0 > Res[,10] - qnorm(0.975)*Res[,12] & psi10.0 < Res[,10] + qnorm(0.975)*Res[,12]))

mean(na.omit(Res[,13]))
sd(Res[,13]); mean(sqrt(Res[,11])); mean(Res[, 15])
prop.table(table(psi11.0 > Res[,13] - qnorm(0.975)*Res[,15] & psi11.0 < Res[,13] + qnorm(0.975)*Res[,15]))

mean(na.omit(Res[,16]))
sd(Res[,16]); mean(Res[, 18])
prop.table(table(psi00.0 > Res[,16] - qnorm(0.975)*Res[,18] & psi00.0 < Res[,16] + qnorm(0.975)*Res[,18]))



