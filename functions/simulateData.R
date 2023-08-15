# -----------------------------------------------------------------------------
# Simulation data simple 
# -----------------------------------------------------------------------------
# R code:
# simulate data from longitudinal mediation setting with baseline treatment (A),
# follow-up times t=1,2 where possible dropout (C1, C2), mediators (M1, M2) 
# and covariates (L1, L2) are recorded, and a binary outcome (Y) subject to 
# missingness (RY).
#
simulateData <- function(n, # integer (sample size)
                         betaL1.A=0.50, # numeric (reg coef for effect of A on L_1)
                         betaM1.A=1.00, # numeric (reg coef for effect of A on M_1)
                         betaL2.A=0.25, # numeric (reg coef for effect of A on L_2)
                         betaM2.A=0.75, # numeric (reg coef for effect of A on M_2)
                         betaY.A=0.50, # numeric (reg coef for effect of A on Y)
                         betaY.M=1.00, # numeric (reg coef for effect of M_2 on Y)
                         betaY.AM=0.15, # numeric (reg coef for effect of A M_2 interaction on Y)
                         betaY.L=0.04, # numeric (reg coef for effect of L_2  on Y)
                         alphaY = -2.0, # numeric (intercept i Y model)
                         a=NULL, 
                         a.prime=NULL
                      
){
  L01 <- rnorm(n, mean = 4, sd = 1)
  L02 <- rbinom(n, 1, 0.4)
  A <- rbinom(n, 1, 0.5)
  if(length(a)>0 & length(a.prime)>0){
   C1 <- rep(0, n)
   L1 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*a - 0.1*L02, sd = 1)
   M1 <- rnorm(n, mean = 2.3 + betaM1.A*a.prime  + 0.05*L1 + 0.1*L02, sd = 1)
   C2 <- rep(0, n)
   L2 <- rnorm(n, mean = 0.5 + 0.85*L1 + betaL2.A*a - 0.05*M1 - 0.1*L02, sd = 1)
   M2 <- rnorm(n, mean = 0.5 + M1 + betaM2.A*a.prime + 0.05*L2+ 0.1*L02, sd = 1)
   RY <- rep(1, n)
   Y <- rbinom(n, 1, plogis(alphaY + betaY.A*a + betaY.M*M2 + betaY.AM*a*M2 + betaY.L*L2 + 0.05*L02))
  }  
  else{
    C1 <- rbinom(n, 1, plogis(-2.25 - 0.5*A + 0.03*L01))
    L1 <- rnorm(n, mean = 0.5 + 0.85*L01 + betaL1.A*A - 0.1*L02, sd = 1)
    M1 <- rnorm(n, mean = 2.3 + betaM1.A*A  + 0.05*L1 + 0.1*L02, sd = 1)
    C2 <- rbinom(n, 1, plogis(-2.25 - 0.5*A + 0.03*L1 - 0.05*M1))
    L2 <- rnorm(n, mean = 0.5 + 0.85*L1 + betaL2.A*A - 0.05*M1 - 0.1*L02, sd = 1)
    M2 <- rnorm(n, mean = 0.5 + M1 + betaM2.A*A + 0.05*L2+ 0.1*L02, sd = 1)
    RY <- rbinom(n, 1, plogis(0.6 + 0.2*A + 0.05*M2 + 0.02*L2))
    Y <- rbinom(n, 1, plogis(alphaY + betaY.A*A + betaY.M*M2 + betaY.AM*A*M2 + betaY.L*L2 + 0.05*L02))
  }
  
  ID=c(1:n)
  dat <- data.table::data.table(ID, L01, L02=factor(L02), A=factor(A), C1, L1, M1, C2, L2, M2, RY, Y)
  dat[C1==1, C2:=1]
  dat[C2==1, RY:=0]
  
  return(dat)
}

