# ------------------------------------------------------------------------------
# simulateData.R
# ------------------------------------------------------------------------------
#
simulateData <- function(n, # sample size
                         betaL1.A, # reg coef for effect of A on L_1
                         betaM1.A, # reg coef for effect of A on M_1
                         betaL2.A, # reg coef for effect of A on L_2
                         betaM2.A, # reg coef for effect of A on M_2
                         betaL3.A, # reg coef for effect of A on L_3
                         betaM3.A, # reg coef for effect of A on M_3
                         betaL2.M, # reg coef for effect of M on L2
                         betaL3.M, # reg coef for effect of M on L3
                         betaY.A, # reg coef for effect of A on Y
                         betaY.M, # reg coef for effect of M_3 on Y
                         betaY.L, # reg coef for effect of L_3  on Y
                         a=NULL, 
                         a.prime=NULL
                         
){
  L01 <- rnorm(n, mean = 4, sd = 0.5)
  L02 <- rnorm(n, mean = 100, sd = 20)
  A <- rbinom(n, 1, 0.5)
  
  if(!is.null(a) & !is.null(a.prime)){
    C1 <- rep(0, n)
    L1 <- rnorm(n, mean = 1.0 + 0.7*L01 + betaL1.A*a, sd = 0.4)
    M1 <- rnorm(n, mean = -4.2 + betaM1.A*a.prime + 1.6*L1 - 0.03*L02, sd = 4)
    C2 <- rep(0, n)
    L2 <- rnorm(n, mean = 0.70 + betaL2.A*a + 0.80*L1 + betaL2.M*M1, sd = 0.4)
    M2 <- rnorm(n, mean = -4.0 + betaM2.A*a.prime + 1.15*M1 + 1.10*L2, sd = 3.5)
    C3 <- rep(0,n)
    L3 <- rnorm(n, mean = 0.9 + betaL3.A*a + 0.75*L2 + betaL3.M*M2, sd = 0.4)
    M3 <- rnorm(n, mean = -4.5 + betaM3.A*a.prime + 1.0*M2 + 1.15*L3, sd = 3)
    RY <- rep(1, n)
    Y <- rbinom(n, 1, plogis(3.0 + betaY.A*a + betaY.M*M3 + betaY.L*L3))
  }  
  else{
    C1 <- rbinom(n, 1, plogis(-3 - 0.2*A))
    L1 <- rnorm(n, mean = 1.0 + 0.7*L01 + betaL1.A*A, sd = 0.4)
    M1 <- rnorm(n, mean = -4.2 + betaM1.A*A + 1.6*L1 - 0.03*L02, sd = 4)
    C2 <- rbinom(n, 1, plogis(-3 - 0.2*A - 0.05*M1))
    L2 <- rnorm(n, mean = 0.70 + betaL2.A*A + 0.80*L1 + betaL2.M*M1, sd = 0.4)
    M2 <- rnorm(n, mean = -4.0 + betaM2.A*A + 1.15*M1 +1.10*L2, sd = 3.5)
    C3 <- rbinom(n, 1, plogis(-3 - 0.2*A - 0.05*M2))
    L3 <- rnorm(n, mean = 0.9 + betaL3.A*A + 0.75*L2  + betaL3.M*M2, sd = 0.4)
    M3 <- rnorm(n, mean = -4.5 + betaM3.A*A + 1.0*M2 + 1.15*L3, sd = 3)
    RY <- rbinom(n, 1, plogis(3.7 - 0.2*A + 0.03*M3 - 0.3*L3))
    Y <- rbinom(n, 1, plogis(3.0 + betaY.A*A + betaY.M*M3 + betaY.L*L3))
  }
  
  ID=c(1:n)
  dat <- data.table::data.table(ID, L01, L02, A=factor(A), C1, L1, M1, 
                                C2, L2, M2, C3, L3, M3, RY, Y)
  dat[C1==1, C2:=1]
  dat[C2==1, C3:=1]
  dat[C3==1, RY:=0]
  
  return(dat)
}
#-------------------------------------------------------------------------------