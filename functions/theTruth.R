theTruth <- function(n,
                     betaL1.A=0.75, 
                     betaL2.A=1.00, 
                     betaY.A=0.75, 
                     betaY.M=0.20, 
                     betaY.AL=0.0,
                     betaY.L=-0.15,
                     alphaY=-1, 
                     coefM1,
                     coefM2,
                     sdM1,
                     sdM2,
                     pi
){
    
      L01 <- rnorm(n, mean = 4, sd = 1)
      L1.a1 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*1 , sd = 1)
      L1.a0 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*0 , sd = 1)
      
      M1.a1.a1 <- rnorm(n, mean = coefM1[1] + coefM1[2]*1  + coefM1[3]*L1.a1 , sd = sdM1)
      M1.a1.a0 <- rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a1 , sd = sdM1)
      M1.a0.a0 <- rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a0 , sd = sdM1)
      M1.a1.star <- pi*rnorm(n, mean = coefM1[1] + coefM1[2]*1  + coefM1[3]*L1.a1 , sd = sdM1)+
        (1-pi)*rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a1 , sd = sdM1)
      M1.a0.star <- pi*rnorm(n, mean = coefM1[1] + coefM1[2]*1  + coefM1[3]*L1.a0 , sd = sdM1)+
        (1-pi)*rnorm(n, mean = coefM1[1] + coefM1[2]*0  + coefM1[3]*L1.a0 , sd = sdM1)
      #M1.star <- pi*M1.a1.a1 + (1-pi)*M1.a0.a0
      L2.a1.a1 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.a1.a1, sd = 1)
      L2.a1.a0 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.a1.a0, sd = 1)
      L2.a0.a0 <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a0 + betaL2.A*0 + 0.2*M1.a0.a0, sd = 1)
      L2.a1.star <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.a1.star, sd = 1)
      L2.a0.star <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a0 + betaL2.A*0 + 0.2*M1.a0.star, sd = 1)
      #L2.a1.star <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a1 + betaL2.A*1 + 0.2*M1.star, sd = 1)
      #L2.a0.star <- rnorm(n, mean = 0.5 + 0.1*L01+ 0.75*L1.a0 + betaL2.A*0 + 0.2*M1.star, sd = 1)
      M2.a1.a1 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.a1 + coefM2[3]*1 +coefM2[4]*L2.a1.a1, sd = sdM2)
      M2.a1.a0 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.a0 + coefM2[3]*0 +coefM2[4]*L2.a1.a0, sd = sdM2)
      M2.a0.a0 <- rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a0.a0 + coefM2[3]*0 +coefM2[4]*L2.a0.a0, sd = sdM2)
      M2.a1.star <- pi*rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.star + coefM2[3]*1 +coefM2[4]*L2.a1.star, sd = sdM2)+
        (1-pi)*rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a1.star + coefM2[3]*0 +coefM2[4]*L2.a1.star, sd = sdM2)
      M2.a0.star <- pi*rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a0.star + coefM2[3]*1 +coefM2[4]*L2.a0.star, sd = sdM2)+
        (1-pi)*rnorm(n, mean = coefM2[1] + coefM2[2]*M1.a0.star + coefM2[3]*0 +coefM2[4]*L2.a0.star, sd = sdM2)
      #M2.star <- pi*M2.a1.a1 + (1-pi)*M2.a0.a0
      Y.a1.a1 <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.a1.a1 + betaY.AL*1*L2.a1.a1 + betaY.L*L2.a1.a1))
      Y.a1.a0 <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.a1.a0 + betaY.AL*1*L2.a1.a0 + betaY.L*L2.a1.a0))
      Y.a0.a0 <- rbinom(n, 1, plogis(alphaY + betaY.A*0 + betaY.M*M2.a0.a0 + betaY.AL*0*L2.a0.a0 + betaY.L*L2.a0.a0))
      Y.a1.star <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.a1.star + betaY.AL*1*L2.a1.star + betaY.L*L2.a1.star))
      Y.a0.star <- rbinom(n, 1, plogis(alphaY + betaY.A*0 + betaY.M*M2.a0.star + betaY.AL*0*L2.a0.star + betaY.L*L2.a0.star))
      #Y.a1.star <- rbinom(n, 1, plogis(alphaY + betaY.A*1 + betaY.M*M2.star + betaY.AL*1*L2.a1.star + betaY.L*L2.a1.star))
      #Y.a0.star <- rbinom(n, 1, plogis(alphaY + betaY.A*0 + betaY.M*M2.star + betaY.AL*0*L2.a0.star + betaY.L*L2.a0.star))
      
  psi11.0 <- mean(Y.a1.a1)
  psi10.0 <- mean(Y.a1.a0)
  psi00.0 <- mean(Y.a0.a0)
  SDE.0 <- psi10.0-psi00.0
  SIE.0 <- psi11.0-psi10.0
  OE.0 <- psi11.0-psi00.0
  GSDE.0 <- mean(Y.a1.star)-mean(Y.a0.star)
  SDE.prop.0 <-SDE.0/OE.0
  SIE.prop.0 <-SIE.0/OE.0
  
  return(data.frame(psi11.true=psi11.0, psi10.true=psi10.0, psi00.true = psi00.0,
                    sde.true = SDE.0, sie.true = SIE.0, oe.true = OE.0, gsde.true = GSDE.0,
                    sde.prop.true = SDE.prop.0, sie.prop.true = SIE.prop.0))
}

