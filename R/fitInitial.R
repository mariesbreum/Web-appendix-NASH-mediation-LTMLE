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
  
  # compute fitted value pi
  pi <- data[, mean(A==a)]
  
  # Compute fitted values delta_k
  if(is.null(Clearner)){
    lrn_C <- Lrnr_glm$new()
  }
  else{
    lrn_C <- Clearner
  }
  
  data[, paste0("delta.",1) := 1 - lrn_C$train(make_task(data, Cmodel[[1]]))$predict()]
  for(i in 2:K){
    set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("delta.",i), value = data[data[[Cnodes[i-1]]]==0, ][[paste0("delta.",i-1)]]*
          (1-lrn_C$train(make_task(data[data[[Cnodes[i-1]]]==0,], Cmodel[[i]]))$predict())) 
  }
  
  # Compute fitted values p_RY
  if(is.null(RYlearner)){
    lrn_RY <- Lrnr_glm$new()
  }
  else{
    lrn_RY <- RYlearner
  }
  
  data[data[[Cnodes[K]]]==0, pRY:= lrn_RY$train(make_task(data[data[[Cnodes[K]]]==0, ], RYmodel))$predict()]
  
  
  # Compute fitted values pM and G
  nd.a <- copy(data)[, A:=rep(a, n)]
  nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
  
  data[data[[Cnodes[1]]]==0, paste0("pM.cum.", 1) := fitG[[1]]$predict()]
  data[data[[Cnodes[1]]]==0, paste0("g.a.cum.", 1) := fitG[[1]]$predict(make_task(nd.a[nd.a[[Cnodes[1]]]==0,], Mmodel[[1]]))]
  data[data[[Cnodes[1]]]==0, paste0("g.aprime.cum.", 1) := fitG[[1]]$predict(make_task(nd.aprime[nd.aprime[[Cnodes[1]]]==0,], Mmodel[[1]]))]

  for(i in 2:K){
    set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("pM.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]]* 
          fitG[[i]]$predict())
    set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.a.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.a.cum.", i-1)]]* 
          fitG[[i]]$predict(make_task(nd.a[nd.a[[Cnodes[i]]]==0,], Mmodel[[i]])))
    set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.aprime.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.aprime.cum.", i-1)]]* 
          fitG[[i]]$predict(make_task(nd.aprime[nd.aprime[[Cnodes[i]]]==0,], Mmodel[[i]])))
  }
  

  # Compute weights H_k
  data[, paste0("H.a.ga.", 1):= 0]
  set(data, i = which(data[[Anode]]==a & data[[Cnodes[1]]]==0), j=paste0("H.a.ga.", 1), value = 1/pi * 
        1/data[data[[Anode]]==a & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  data[, paste0("H.a.gaprime.", 1):= 0]
  set(data, i = which(data[[Anode]]==a & data[[Cnodes[1]]]==0), j=paste0("H.a.gaprime.", 1), value = 1/pi * 
        1/data[data[[Anode]]==a & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  data[, paste0("H.aprime.gaprime.", 1):= 0]
  set(data, i = which(data[[Anode]]==a.prime & data[[Cnodes[1]]]==0), j=paste0("H.aprime.gaprime.", 1), value = 1/(1-pi) * 
        1/data[data[[Anode]]==a.prime & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  
  for(i in 2:K){
    data[, paste0("H.a.ga.", i):= 0 ]
    set(data, i = which(data[[Anode]]==a & data[[Cnodes[i]]]==0), j = paste0("H.a.ga.", i), value = 1/pi * 
          1/data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("g.a.cum.", i-1)]]/data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    data[,paste0("H.a.gaprime.",i):= 0]
    set(data, i = which(data[[Anode]]==a & data[[Cnodes[i]]]==0), j = paste0("H.a.gaprime.", i), value = 1/pi * 
          1/data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("g.aprime.cum.", i-1)]]/data[data[[Anode]]==a & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    data[,paste0("H.aprime.gaprime.", i):= 0]
    set(data, i = which(data[[Anode]]==a.prime & data[[Cnodes[i]]]==0), j = paste0("H.aprime.gaprime.", i), value = 1/(1-pi) * 
          1/data[data[[Anode]]==a.prime & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a.prime & data[[Cnodes[i]]]==0,][[paste0("g.aprime.cum.", i-1)]]/data[data[[Anode]]==a.prime & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    
  }
  
  data[, paste0("H.a.ga.", K+1):= 0]
  set(data, i = which(data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.a.ga.", K+1), value = 1/pi * 
        1/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.a.cum.", K)]]/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  data[, paste0("H.a.gaprime.", K+1):= 0] 
  set(data, i = which(data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.a.gaprime.", K+1), value = 1/pi * 
        1/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.aprime.cum.", K)]]/data[data[[Anode]]==a & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  data[, paste0("H.aprime.gaprime.", K+1):= 0]
  set(data, i = which(data[[Anode]]==a.prime & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.aprime.gaprime.", K+1), value = 1/(1-pi) * 
        1/data[data[[Anode]]==a.prime & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a.prime & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a.prime & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.aprime.cum.", K)]]/data[data[[Anode]]==a.prime & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  
  out_columns <-c(sapply(1:(K+1), function(i) paste0("H.a.ga.", i)), sapply(1:(K+1), function(i) paste0("H.a.gaprime.", i)),
                  sapply(1:(K+1), function(i) paste0("H.aprime.gaprime.", i)))
  data[,..out_columns]

}

