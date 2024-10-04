# -----------------------------------------------------------------------------
# theTruth.R
# -----------------------------------------------------------------------------
# 
theTruth <- function(n,
                     betaL1.A, 
                     betaM1.A, 
                     betaL2.A,
                     betaM2.A,
                     betaL3.A,
                     betaM3.A,
                     betaL2.M,
                     betaL3.M,
                     betaY.A, 
                     betaY.M, 
                     betaY.L,
                     fitg,
                     pi=0.5
){
  
  L01 <- rnorm(n, mean = 4, sd = 0.5)
  L02 <- rnorm(n, mean = 100, sd = 20)
  
  L1.a1 <- rnorm(n, mean = 1.0 + 0.70*L01 + betaL1.A*1 , sd = 0.4)
  L1.a0 <- rnorm(n, mean = 1.0 + 0.70*L01 + betaL1.A*0 , sd = 0.4)
  
  M1.a1.ga1 <- rnorm(n, mean = predict(fitg[[1]], newdata = data.frame(A=as.factor(rep(1,n)), L1=L1.a1, L02=L02)), 
                     sd = sd(fitg[[1]]$residuals))
  M1.a1.ga0 <- rnorm(n, mean = predict(fitg[[1]], newdata = data.frame(A=as.factor(rep(0,n)), L1=L1.a1, L02=L02)), 
                     sd = sd(fitg[[1]]$residuals))
  M1.a0.ga0 <- rnorm(n, mean =predict(fitg[[1]], newdata = data.frame(A=as.factor(rep(0,n)), L1=L1.a0, L02=L02)), 
                     sd = sd(fitg[[1]]$residuals))
  M1.a0.ga1 <- rnorm(n, mean = predict(fitg[[1]], newdata = data.frame(A=as.factor(rep(1,n)), L1=L1.a0, L02=L02)), 
                     sd = sd(fitg[[1]]$residuals))
  
  L2.a1.ga1 <- rnorm(n, mean = 0.7 + betaL2.A*1 + 0.80*L1.a1 + betaL2.M*M1.a1.ga1, sd = 0.4)
  L2.a1.ga0 <- rnorm(n, mean = 0.7 + betaL2.A*1 + 0.80*L1.a1 + betaL2.M*M1.a1.ga0, sd = 0.4)
  L2.a0.ga0 <- rnorm(n, mean = 0.7 + betaL2.A*0 + 0.80*L1.a0 + betaL2.M*M1.a0.ga0, sd = 0.4)
  L2.a0.ga1 <- rnorm(n, mean = 0.7 + betaL2.A*0 + 0.80*L1.a0 + betaL2.M*M1.a0.ga1, sd = 0.4)
  
  M2.a1.g1a0.g2a0 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(0,n)), M1=M1.a1.ga0, L2=L2.a1.ga0)),
                           sd = sd(fitg[[2]]$residuals))
  M2.a1.g1a0.g2a1 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(1,n)), M1=M1.a1.ga0, L2=L2.a1.ga0)),
                           sd = sd(fitg[[2]]$residuals))
  M2.a1.g1a1.g2a0 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(0,n)), M1=M1.a1.ga1, L2=L2.a1.ga1)),
                           sd = sd(fitg[[2]]$residuals))
  M2.a1.g1a1.g2a1 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(1,n)), M1=M1.a1.ga1, L2=L2.a1.ga1)),
                     sd = sd(fitg[[2]]$residuals))
  M2.a0.g1a0.g2a0 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(0,n)), M1=M1.a0.ga0, L2=L2.a0.ga0)),
                     sd = sd(fitg[[2]]$residuals))
  M2.a0.g1a0.g2a1 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(1,n)), M1=M1.a0.ga0, L2=L2.a0.ga0)),
                           sd = sd(fitg[[2]]$residuals))
  M2.a0.g1a1.g2a0 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(0,n)), M1=M1.a0.ga1, L2=L2.a0.ga1)),
                     sd = sd(fitg[[2]]$residuals))
  M2.a0.g1a1.g2a1 <- rnorm(n, mean = predict(fitg[[2]], newdata = data.frame(A=as.factor(rep(1,n)), M1=M1.a0.ga1, L2=L2.a0.ga1)),
                         sd = sd(fitg[[2]]$residuals))
  
  L3.a1.g1a0.g2a0 <- rnorm(n, mean = 0.9 + betaL3.A*1 + 0.75*L2.a1.ga0 + betaL3.M*M2.a1.g1a0.g2a0, sd = 0.4)
  L3.a1.g1a0.g2a1 <- rnorm(n, mean = 0.9 + betaL3.A*1 + 0.75*L2.a1.ga0 + betaL3.M*M2.a1.g1a0.g2a1, sd = 0.4)
  L3.a1.g1a1.g2a0 <- rnorm(n, mean = 0.9 + betaL3.A*1 + 0.75*L2.a1.ga1 + betaL3.M*M2.a1.g1a1.g2a0, sd = 0.4)
  L3.a1.g1a1.g2a1 <- rnorm(n, mean = 0.9 + betaL3.A*1 + 0.75*L2.a1.ga1 + betaL3.M*M2.a1.g1a1.g2a1, sd = 0.4)
  L3.a0.g1a0.g2a0 <- rnorm(n, mean = 0.9 + betaL3.A*0 + 0.75*L2.a0.ga0 + betaL3.M*M2.a0.g1a0.g2a0, sd = 0.4)
  L3.a0.g1a0.g2a1 <- rnorm(n, mean = 0.9 + betaL3.A*0 + 0.75*L2.a0.ga0 + betaL3.M*M2.a0.g1a0.g2a1, sd = 0.4)
  L3.a0.g1a1.g2a0 <- rnorm(n, mean = 0.9 + betaL3.A*0 + 0.75*L2.a0.ga1 + betaL3.M*M2.a0.g1a1.g2a0, sd = 0.4)
  L3.a0.g1a1.g2a1 <- rnorm(n, mean = 0.9 + betaL3.A*0 + 0.75*L2.a0.ga1 + betaL3.M*M2.a0.g1a1.g2a1, sd = 0.4)
  
  
  
  M3.a1.g1a0.g2a0.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a1.g1a0.g2a0, L3=L3.a1.g1a0.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a0.g2a0.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a1.g1a0.g2a0, L3=L3.a1.g1a0.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a0.g2a1.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a1.g1a0.g2a1, L3=L3.a1.g1a0.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a0.g2a1.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a1.g1a0.g2a1, L3=L3.a1.g1a0.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a1.g2a0.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a1.g1a1.g2a0, L3=L3.a1.g1a1.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a1.g2a0.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a1.g1a1.g2a0, L3=L3.a1.g1a1.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a1.g2a1.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a1.g1a1.g2a1, L3=L3.a1.g1a1.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a1.g1a1.g2a1.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a1.g1a1.g2a1, L3=L3.a1.g1a1.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a0.g2a0.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a0.g1a0.g2a0, L3=L3.a0.g1a0.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a0.g2a0.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a0.g1a0.g2a0, L3=L3.a0.g1a0.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a0.g2a1.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a0.g1a0.g2a1, L3=L3.a0.g1a0.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a0.g2a1.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a0.g1a0.g2a1, L3=L3.a0.g1a0.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a1.g2a0.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a0.g1a1.g2a0, L3=L3.a0.g1a1.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a1.g2a0.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a0.g1a1.g2a0, L3=L3.a0.g1a1.g2a0)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a1.g2a1.g3a0 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(0,n)), M2=M2.a0.g1a1.g2a1, L3=L3.a0.g1a1.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  M3.a0.g1a1.g2a1.g3a1 <- rnorm(n, mean = predict(fitg[[3]], newdata = data.frame(A=as.factor(rep(1,n)), M2=M2.a0.g1a1.g2a1, L3=L3.a0.g1a1.g2a1)),
                                sd = sd(fitg[[3]]$residuals))
  
  Y.a1.g1a0.g2a0.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a0.g2a0.g3a0 + betaY.L*L3.a1.g1a0.g2a0))
  Y.a1.g1a0.g2a0.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a0.g2a0.g3a1 + betaY.L*L3.a1.g1a0.g2a0))
  Y.a1.g1a0.g2a1.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a0.g2a1.g3a0 + betaY.L*L3.a1.g1a0.g2a1))
  Y.a1.g1a0.g2a1.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a0.g2a1.g3a1 + betaY.L*L3.a1.g1a0.g2a1))
  Y.a1.g1a1.g2a0.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a1.g2a0.g3a0 + betaY.L*L3.a1.g1a1.g2a0))
  Y.a1.g1a1.g2a0.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a1.g2a0.g3a1 + betaY.L*L3.a1.g1a1.g2a0))
  Y.a1.g1a1.g2a1.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a1.g2a1.g3a0 + betaY.L*L3.a1.g1a1.g2a1))
  Y.a1.g1a1.g2a1.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*1 + betaY.M*M3.a1.g1a1.g2a1.g3a1 + betaY.L*L3.a1.g1a1.g2a1))
  Y.a0.g1a0.g2a0.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a0.g2a0.g3a0 + betaY.L*L3.a0.g1a0.g2a0))
  Y.a0.g1a0.g2a0.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a0.g2a0.g3a1 + betaY.L*L3.a0.g1a0.g2a0))
  Y.a0.g1a0.g2a1.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a0.g2a1.g3a0 + betaY.L*L3.a0.g1a0.g2a1))
  Y.a0.g1a0.g2a1.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a0.g2a1.g3a1 + betaY.L*L3.a0.g1a0.g2a1))
  Y.a0.g1a1.g2a0.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a1.g2a0.g3a0 + betaY.L*L3.a0.g1a1.g2a0))
  Y.a0.g1a1.g2a0.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a1.g2a0.g3a1 + betaY.L*L3.a0.g1a1.g2a0))
  Y.a0.g1a1.g2a1.g3a0 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a1.g2a1.g3a0 + betaY.L*L3.a0.g1a1.g2a1))
  Y.a0.g1a1.g2a1.g3a1 <- rbinom(n, 1, plogis(3.0 + betaY.A*0 + betaY.M*M3.a0.g1a1.g2a1.g3a1 + betaY.L*L3.a0.g1a1.g2a1))
  
  
  psi11 <- mean(Y.a1.g1a1.g2a1.g3a1)
  psi10 <- mean(Y.a1.g1a0.g2a0.g3a0)
  psi00 <- mean(Y.a0.g1a0.g2a0.g3a0)
  psi1g <- pi^3*mean(Y.a1.g1a1.g2a1.g3a1) +  pi^3*mean(Y.a1.g1a0.g2a1.g3a1) + pi^3*mean(Y.a1.g1a1.g2a0.g3a1) + pi^3*mean(Y.a1.g1a1.g2a1.g3a0)+ 
    pi^3*mean(Y.a1.g1a0.g2a0.g3a1) +  pi^3*mean(Y.a1.g1a1.g2a0.g3a0) + pi^3*mean(Y.a1.g1a0.g2a1.g3a0) + pi^3*mean(Y.a1.g1a0.g2a0.g3a0)
  psi0g <- pi^3*mean(Y.a0.g1a1.g2a1.g3a1) +  pi^3*mean(Y.a0.g1a0.g2a1.g3a1) + pi^3*mean(Y.a0.g1a1.g2a0.g3a1) + pi^3*mean(Y.a0.g1a1.g2a1.g3a0)+ 
    pi^3*mean(Y.a0.g1a0.g2a0.g3a1) +  pi^3*mean(Y.a0.g1a1.g2a0.g3a0) + pi^3*mean(Y.a0.g1a0.g2a1.g3a0) + pi^3*mean(Y.a0.g1a0.g2a0.g3a0)
  
  ide <- psi10-psi00
  iie <- psi11-psi10
  oe <- psi11-psi00
  pm <- ide/oe
  gide <- psi1g-psi0g
  
  ide.OR <- (psi10/(1-psi10)) / (psi00/(1-psi00))
  iie.OR <- (psi11/(1-psi11)) / (psi10/(1-psi10))
  oe.OR  <- (psi11/(1-psi11)) / (psi00/(1-psi00))
  gide.OR  <- (psi1g/(1-psi1g)) / (psi0g/(1-psi0g))
  
  return(data.frame(psi11.true=psi11, psi10.true=psi10, psi00.true = psi00, psi1g.true=psi1g, 
                    psi0g.true = psi0g, ide.true = ide, iie.true = iie, oe.true = oe, pm.true = pm,
                    gide.true = gide, ide.OR.true = ide.OR, iie.OR.true = iie.OR, oe.OR.true = oe.OR,
                    gide.OR.true = gide.OR))
}

#-------------------------------------------------------------------------------