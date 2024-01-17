# -----------------------------------------------------------------------------
# theTruth
# -----------------------------------------------------------------------------
# Description:
# This function computes the true value of the SDE and SIE. 
#
theTruth <- function(n,
                     betaL1.A=0.75, 
                     betaM1.A=0.75, 
                     betaL2.A=1.00,
                     betaM2.A=1.00,
                     betaY.A=0.75, 
                     betaY.M=0.20, 
                     betaY.AL=0.0, 
                     betaY.L=-0.15,
                     alphaY=-1, 
                     fitg=NULL
){
  
  if(is.null(fitg)){
    coefM1 = c(2.3, betaM1.A, -0.2)
    coefM2 = c(0.5, 0.9, betaM2.A, -0.2)
    sdM1 = 1
    sdM2 = 1
  }
  else{
    coefM1 = fitg[[1]]$coefficients 
    coefM2 = fitg[[2]]$coefficients
    sdM1 = sd(fitg[[1]]$residuals) 
    sdM2 = sd(fitg[[2]]$residuals) 
  }
    
  L01 <- rnorm(n, mean = 4, sd = 1)
  L1.a1 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*1 , sd = 1)
  L1.a0 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*0 , sd = 1)
  M1.a1.a1 <- rnorm(n, mean = coefM1[1] + coefM1[2]*1  + coefM1[3]*L1.a1 , sd = sdM1)
  M1.a1.a0 <- rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a1 , sd = sdM1)
  M1.a0.a0 <- rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a0 , sd = sdM1)
  L2.a1.a1 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.a1.a1, sd = 1)
  L2.a1.a0 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.a1.a0, sd = 1)
  L2.a0.a0 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a0 + betaL2.A*0 + 0.2*M1.a0.a0, sd = 1)
  M2.a1.a1 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.a1 + coefM2[3]*1 +coefM2[4]*L2.a1.a1, sd = sdM2)
  M2.a1.a0 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.a0 + coefM2[3]*0 +coefM2[4]*L2.a1.a0, sd = sdM2)
  M2.a0.a0 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a0.a0 + coefM2[3]*0 +coefM2[4]*L2.a0.a0, sd = sdM2)
  Y.a1.a1 <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.a1.a1 + betaY.AL*1*L2.a1.a1 + betaY.L*L2.a1.a1))
  Y.a1.a0 <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.a1.a0 + betaY.AL*1*L2.a1.a0 + betaY.L*L2.a1.a0))
  Y.a0.a0 <- rbinom(n, 1, plogis(alphaY + betaY.A*0 + betaY.M*M2.a0.a0 + betaY.AL*0*L2.a0.a0 + betaY.L*L2.a0.a0))

  psi11.0 <- mean(Y.a1.a1)
  psi10.0 <- mean(Y.a1.a0)
  psi00.0 <- mean(Y.a0.a0)
  sde.0 <- psi10.0-psi00.0
  sie.0 <- psi11.0-psi10.0
  oe.0 <- psi11.0-psi00.0
  pm.0 <- sde.0/oe.0

  sde.OR.0 <- (psi10.0/(1-psi10.0)) / (psi00.0/(1-psi00.0))
  sie.OR.0 <- (psi11.0/(1-psi11.0)) / (psi10.0/(1-psi10.0))
  oe.OR.0  <- (psi11.0/(1-psi11.0)) / (psi00.0/(1-psi00.0))
  sde.logOR.0 <- log(sde.OR.0)
  sie.logOR.0 <- log(sie.OR.0)
  oe.logOR.0 <- log(oe.OR.0)
  pm.logOR.0 <- sde.logOR.0/oe.logOR.0
  
  return(data.frame(psi11.true=psi11.0, psi10.true=psi10.0, psi00.true = psi00.0,
                    sde.true = sde.0, sie.true = sie.0, oe.true = oe.0, 
                    pm.true = pm.0,
                    sde.OR.true = sde.OR.0, sie.OR.true = sie.OR.0, 
                    oe.OR.true = oe.OR.0, sde.logOR.true = sde.logOR.0, sie.logOR.true = sie.logOR.0, 
                    oe.logOR.true = oe.logOR.0, pm.logOR.true=pm.logOR.0))
}

