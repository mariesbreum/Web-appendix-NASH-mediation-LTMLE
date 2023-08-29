theTruth <- function(n,
                     betaL1.A=0.75, 
                     betaL2.A=1.00, 
                     betaY.A=0.50, 
                     betaY.M=0.30, 
                     betaY.AL=0.1,
                     betaY.L=-0.1,
                     alphaY=-1, 
                     coefM1,
                     coefM2,
                     sdM1,
                     sdM2
){
    
    Y.a1.a0 <- function(a1, a0){
      L01 <- rnorm(n, mean = 4, sd = 1)
      L1 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*a1 , sd = 1)
      M1 <- rnorm(n, mean = coefM1[1] + coefM1[2]*a0  + coefM1[3]*L1 , sd = sdM1)
      L2 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1 + betaL2.A*a1 + 0.2*M1, sd = 1)
      M2 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1 + coefM2[3]*a0 +coefM2[4]*L2, sd = sdM2)
      Y <- rbinom(n, 1, plogis(alphaY + betaY.A*a1 + betaY.M*M2 + betaY.AL*a1*L2 + betaY.L*L2))
      return(mean(Y))
    }

  psi11.0 <- Y.a1.a0(1,1)
  psi10.0 <- Y.a1.a0(1,0)
  psi00.0 <- Y.a1.a0(0,0)
  SDE.0 <- psi10.0-psi00.0
  SIE.0 <- psi11.0-psi10.0
  OE.0 <- psi11.0-psi00.0
  SDE.prop.0 <-SDE.0/OE.0
  SIE.prop.0 <-SIE.0/OE.0
  
  return(data.frame(psi11.true=psi11.0, psi10.true=psi10.0, psi00.true = psi00.0,
                    sde.true = SDE.0, sie.true = SIE.0, oe.true = OE.0,
                    sde.prop.true = SDE.prop.0, sie.prop.true = SIE.prop.0))
}

