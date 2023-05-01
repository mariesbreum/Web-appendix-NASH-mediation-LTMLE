# -----------------------------------------------------------------------------
# Functions - initial estimates
# -----------------------------------------------------------------------------
# R code:
# This function returns initial estimates of the clever covariates/weights for 
# the ltmle algorithm. 


fitInitial <- function(data, 
                       t, 
                       Anode,
                       Cnodes,
                       RYnode,
                       Cmodel, 
                       Mmodel,
                       RYmodel, 
                       a,
                       a.prime,
                       Clearner, 
                       RYlearner,
                       fitG
                       ){
  
  data <- copy(data)
  K <- length(t)
  n <- nrow(data)
  
  # Compute fitted values delta_k
  if(is.null(Clearner)){
    lrn_C <- Lrnr_glm$new()
  }
  else{
    lrn_C <- Clearner
  }
  
  data[, paste0("delta.",1) := 1 - lrn_C$train(make_task(data, Cmodel[[1]]))$predict()]
  for(i in 2:K){
  data[get(Cnodes[i-1])==0, paste0("delta.",i) := get(paste0("delta.",i-1)) * (1-
         lrn_C$train(make_task(data[get(Cnodes[i-1])==0,], Cmodel[[i]]))$predict())]
  }
  
  # Compute fitted values p_RY
  if(is.null(RYlearner)){
    lrn_RY <- Lrnr_glm$new()
  }
  else{
    lrn_RY <- RYlearner
  }
  
  data[get(Cnodes[K])==0, pRY:= lrn_RY$train(make_task(data[get(Cnodes[K])==0, ], RYmodel))$predict()]
  
  nd.a <- copy(data)[, A:=rep(a, n)]
  nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
  
  data[get(Cnodes[1])==0, paste0("pM.cum.", 1) := fitG[[1]]$predict()]
  data[get(Cnodes[1])==0, paste0("g.a.cum.", 1) := fitG[[1]]$predict(make_task(nd.a[get(Cnodes[1])==0,], Mmodel[[1]]))]
  data[get(Cnodes[1])==0, paste0("g.aprime.cum.", 1) := fitG[[1]]$predict(make_task(nd.aprime[get(Cnodes[1])==0,], Mmodel[[1]]))]

  
  for(i in 2:K){
    data[get(Cnodes[i])==0, paste0("pM.cum.", i) := get(paste0("pM.cum.", i-1)) * 
           fitG[[i]]$predict()]
    data[get(Cnodes[i])==0, paste0("g.a.cum.", i) := get(paste0("g.a.cum.", i-1)) * 
           fitG[[i]]$predict(make_task(nd.a[get(Cnodes[i])==0,], Mmodel[[i]]))]
    data[get(Cnodes[i])==0, paste0("g.aprime.cum.", i) := get(paste0("g.aprime.cum.", i-1)) * 
           fitG[[i]]$predict(make_task(nd.aprime[get(Cnodes[i])==0,], Mmodel[[i]]))]
    
  }
  
  pi <- data[, mean(A==a)]
  
  # Compute weights H_k
  data[, paste0("H.a.ga.", 1):= 0]
  data[get(Anode)==a & get(Cnodes[1])==0, paste0("H.a.ga.", 1):= 1/pi * 1/get(paste0("delta.", 1))]
  data[, paste0("H.a.gaprime.", 1):= 0]
  data[get(Anode)==a & get(Cnodes[1])==0, paste0("H.a.gaprime.", 1):= 1/pi* 1/get(paste0("delta.", 1))]
  data[, paste0("H.aprime.gaprime.", 1):= 0]
  data[get(Anode)==a.prime & get(Cnodes[1])==0, paste0("H.aprime.gaprime.", 1):= 1/(1-pi) * 1/get(paste0("delta.", 1))]
  
  for(i in 2:K){
    data[, paste0("H.a.ga.", i):= 0 ]
    data[get(Anode)==a & get(Cnodes[i])==0, paste0("H.a.ga.", i):= 1/pi * 1/get(paste0("delta.", i)) * 
           get(paste0("g.a.cum.", i-1))/get(paste0("pM.cum.", i-1)) ]
    data[,paste0("H.a.gaprime.",i):= 0]
    data[get(Anode)==a & get(Cnodes[i])==0,paste0("H.a.gaprime.",i):= 1/pi * 1/get(paste0("delta.", i)) * 
           get(paste0("g.aprime.cum.", i-1))/get(paste0("pM.cum.", i-1))]
    data[,paste0("H.aprime.gaprime.", i):= 0]
    data[get(Anode)==a.prime & get(Cnodes[i])==0,paste0("H.aprime.gaprime.", i):= 1/(1-pi) * 1/get(paste0("delta.", i)) * 
           get(paste0("g.aprime.cum.", i-1))/get(paste0("pM.cum.", i-1))]
    
  }
  data[, paste0("H.a.ga.", K+1):= 0]
  data[get(Anode)==a & get(Cnodes[K])==0 & get(RYnode)==1, paste0("H.a.ga.", K+1):= 1/pi * 1/get(paste0("delta.", K)) * 1/pRY *
          get(paste0("g.a.cum.", K))/get(paste0("pM.cum.", K)) ]
  data[, paste0("H.a.gaprime.", K+1):= 0] 
  data[get(Anode)==a & get(Cnodes[K])==0 & get(RYnode)==1, paste0("H.a.gaprime.", K+1):= 1/pi * 1/get(paste0("delta.", K)) * 1/pRY *
          get(paste0("g.aprime.cum.", K))/get(paste0("pM.cum.", K))] 
  data[, paste0("H.aprime.gaprime.", K+1):= 0]
  data[get(Anode)==a.prime & get(Cnodes[K])==0 & get(RYnode)==1, paste0("H.aprime.gaprime.", K+1):= 1/(1-pi) * 1/get(paste0("delta.", K)) * 1/pRY *
          get(paste0("g.aprime.cum.", K))/get(paste0("pM.cum.", K))]
  
  out_columns <-c(sapply(1:(K+1), function(i) paste0("H.a.ga.", i)), sapply(1:(K+1), function(i) paste0("H.a.gaprime.", i)),
                  sapply(1:(K+1), function(i) paste0("H.aprime.gaprime.", i)))
  data[,..out_columns]

}

