# -----------------------------------------------------------------------------
# fitInitial.R
# -----------------------------------------------------------------------------
# Description:
# This function returns initial estimates of the clever covariates/weights for 
# the ltmle algorithm. 
# 
fitInitial <- function(data,  # data table or data frame
                       Anode, # character (baseline treatment variable)
                       L0nodes,
                       Cnodes,# char vector (censoring variables)
                       Lnodes,
                       Mnodes, # char (outcome)
                       RYnode, # char (outcome missingness)
                       Ynode,
                       Cmodel,  # list of models for C_i
                       gmodel, # list of models for g_i
                       RYmodel, # model for R.Y
                       a1, # level of treatment var corresponding to a
                       a0, # level of treatment var corresponding to a'
                       Clearner, 
                       RYlearner,
                       glearner,
                       Mlearner,
                       fitg, # fitted model g
                       fitM, # fitted model M
                       pi # propensity score
){
  
  data <- copy(data)
  
  # set-up
  K <- length(Mnodes)
  n <- nrow(data)
  
  if(is.null(Clearner)){
    Clearner <- Lrnr_glm$new()
  }
  if(is.null(RYlearner)){
    RYlearner <- Lrnr_glm$new()
  }
  
  # Compute pC
  data[, paste0("pC.",1) := 1 - Clearner$train(make_task(data, Cmodel[[1]]))$predict()]
  for(i in 2:K){
    set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("pC.",i), value = 
          1-Clearner$train(make_task(data[data[[Cnodes[i-1]]]==0,], Cmodel[[i]]))$predict()) 
  }
  
  # Compute fitted values pM and g
  vcols <- c(L0nodes, Cnodes, Lnodes, Mnodes, RYnode, Ynode)
  nd.a1 <- data[,..vcols]
  set(nd.a1, j=paste0(Anode), value = factor(rep(a1,n), levels=c(a0,a1)))
  nd.a0 <- data[,..vcols]
  set(nd.a0, j=paste0(Anode), value = factor(rep(a0,n), levels=c(a0,a1)))
  
  
  for(k in 1:K){
    if(is.null(Mlearner)){
      mu.m <- predict(fitM[[k]])
      set(data, i=which(data[[Cnodes[k]]]==0), j=paste0("pM.", k), value = 
            dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], mean=mu.m, sd=sd(fitM[[k]]$residuals)))
      
      mu.g.a1 <- predict(fitg[[k]], newdata=nd.a1[nd.a1[[Cnodes[k]]]==0,])
      set(data, i = which(data[[Cnodes[k]]]==0), j=paste0("g.a1.", k), value = 
            dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], mean=mu.g.a1, sd=sd(fitg[[k]]$residuals)))

      mu.g.a0 <- predict(fitg[[k]], newdata=nd.a0[nd.a0[[Cnodes[k]]]==0,])
      set(data, i= which(data[[Cnodes[k]]]==0), j=paste0("g.a0.", k), value = 
            dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], mean=mu.g.a0, sd=sd(fitg[[k]]$residuals)))
    }
    else{
      set(data, i=which(data[[Cnodes[k]]]==0), j=paste0("pM.", k), value=fitM[[k]]$predict()$likelihood)
      set(data, i=which(data[[Cnodes[k]]]==0), j=paste0("g.a1.", k), value =
            fitg[[k]]$predict(make_task(nd.a1[nd.a1[[Cnodes[k]]]==0,], gmodel[[k]]))$likelihood)
      set(data, i=which(data[[Cnodes[k]]]==0), j=paste0("g.a0.", k), value= 
            fitg[[k]]$predict(make_task(nd.a0[nd.a0[[Cnodes[k]]]==0,], gmodel[[k]]))$likelihood)
    }
    set(data, j = paste0("g.", k), value =
          pi*data[[paste0("g.a1.", k)]] + (1-pi)*data[[paste0("g.a0.", k)]])
    
  }
  
  # Compute weights H_k
  set(data, j=paste0("H.a1.ga1.", 1), value = (1/pi) * 
        (1*(data[[Anode]]==a1 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a1.ga0.", 1), value = (1/pi) * 
        (1*(data[[Anode]]==a1 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a0.ga0.", 1), value = (1/(1-pi)) * 
        (1*(data[[Anode]]==a0 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a1.g.", 1), value = (1/pi) * 
        (1*(data[[Anode]]==a1 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a0.g.", 1), value = (1/(1-pi)) * 
        (1*(data[[Anode]]==a0 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  
  
  for(i in 2:K){
    set(data, j = paste0("H.a1.ga1.", i), value = data[[paste0("H.a1.ga1.", i-1)]]*
     (1*(data[[Cnodes[i]]]==0)/data[[paste0("pC.",i)]])*
       (data[[paste0("g.a1.", i-1)]]/data[[paste0("pM.", i-1)]]))
    set(data, j = paste0("H.a1.ga0.", i), value = data[[paste0("H.a1.ga0.", i-1)]]*
          (1*(data[[Cnodes[i]]]==0)/data[[paste0("pC.",i)]])*
          (data[[paste0("g.a0.", i-1)]]/data[[paste0("pM.", i-1)]]))
    set(data, j = paste0("H.a0.ga0.", i), value = data[[paste0("H.a0.ga0.", i-1)]]*
          (1*(data[[Cnodes[i]]]==0)/data[[paste0("pC.",i)]])*
          (data[[paste0("g.a0.", i-1)]]/data[[paste0("pM.", i-1)]]))
    set(data, j = paste0("H.a1.g.", i), value = data[[paste0("H.a1.g.", i-1)]]*
          (1*(data[[Cnodes[i]]]==0)/data[[paste0("pC.",i)]])*
          (data[[paste0("g.", i-1)]]/data[[paste0("pM.", i-1)]]))
    set(data, j = paste0("H.a0.g.", i), value = data[[paste0("H.a0.g.", i-1)]]*
          (1*(data[[Cnodes[i]]]==0)/data[[paste0("pC.",i)]])*
          (data[[paste0("g.", i-1)]]/data[[paste0("pM.", i-1)]]))
  }
  
  # Compute fitted values p_RY
  set(data, i=which(data[[Cnodes[K]]]==0),j=paste0("pRY"), 
      value=RYlearner$train(make_task(data[data[[Cnodes[K]]]==0, ], RYmodel))$predict())
  
  # Compute weights H_K+1
  set(data, j = paste0("H.a1.ga1.", K+1), value = data[[paste0("H.a1.ga1.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
          (data[[paste0("g.a1.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a1.ga0.", K+1), value = data[[paste0("H.a1.ga0.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.a0.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a0.ga0.", K+1), value = data[[paste0("H.a0.ga0.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.a0.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a1.g.", K+1), value = data[[paste0("H.a1.g.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a0.g.", K+1), value = data[[paste0("H.a0.g.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.", K)]]/data[[paste0("pM.", K)]]))
  
  
  out_columns <-c(sapply(1:(K+1), function(i) paste0("H.a1.ga1.", i)), sapply(1:(K+1), function(i) paste0("H.a1.ga0.", i)),
                  sapply(1:(K+1), function(i) paste0("H.a0.ga0.", i)), sapply(1:(K+1), function(i) paste0("H.a1.g.", i)),
                  sapply(1:(K+1), function(i) paste0("H.a0.g.", i)))
  data[,..out_columns]
  
}
#-------------------------------------------------------------------------------