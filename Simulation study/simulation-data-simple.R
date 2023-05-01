# -----------------------------------------------------------------------------
# Simulation data simple 
# -----------------------------------------------------------------------------
# R code:
# simulate data from longitudinal mediation setting with baseline treatment (A),
# follow-up times t=1,2 where possible dropout (C1, C2), mediators (M1, M2) 
# and covariates (L1, L2) are recorded, and a binary outcome (Y) subject to 
# missingness (RY).
#
simSimple <- function(n, # integer (sample size)
                      betaL = c(0, 0), # numeric vector (reg coef's for effect of A on L_k)
                      betaM = c(-1.75, -1.5), # numeric vector (reg coef's for effect of A on M_k)
                      betaY = 0, # numeric (reg coef for effect of A on Y)
                      alphaY = c(-0.1,-0.3), # numeric vector (reg coef for effect of M_k on Y)
                      a=NULL, 
                      a.prime=NULL
                      
){
  
  
  A <- rbinom(n, 1, 0.5)
  Aprime <- A
  
  if(length(a)>0){
    A <- a
  }
  if(length(a.prime)>0){
    Aprime <- a.prime
  }
  
  muL <- rnorm(0.2, 0.1)
  muM <- rnorm(-0.3, 0.1)
  
  L0 <- rnorm(n, mean=10, sd = 5)
  M0 <- rnorm(n, mean=4, sd = 1)
  C1 <- rbinom(n, 1, 0.03)
  L1 <- rnorm(n, mean = 0.3 + L0 + 0.1*M0 + betaL[1]*A, sd = 1)
  M1 <- rnorm(n, mean =-0.2 + M0 +  betaM[1]*Aprime + 0.05*L1, sd = 1)
  C2 <- rbinom(n, 1, 0.03)
  L2 <- rnorm(n, mean = 0.3 + L1 + betaL[2]*A  + 0.1*M1, sd = 1)
  M2 <- rnorm(n, mean = -0.2 + M1 + betaM[2]*Aprime + 0.075*L2, sd = 1)
  RY <- rbinom(n, 1, plogis(0.4 + 0.02*A + 0.04*M2 +0.01* L2))
  Y <- rbinom(n, 1, plogis(0.2 + betaY*A + 0.02*L0 + 0.04*L2 +alphaY[1]*M1 + alphaY[2]*M2))
  
  ID=c(1:n)
  dat <- data.table::data.table(ID, L0, M0, A, C1, L1, M1, C2, L2, M2,RY, Y)
  dat[C1==1, C2:=1]
  dat[C2==1, RY:=0]
  
  return(dat)
}

