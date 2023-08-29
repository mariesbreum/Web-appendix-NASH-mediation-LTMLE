# -----------------------------------------------------------------------------
# Functions - initial estimates
# -----------------------------------------------------------------------------
# Description:
# This function returns initial estimates of the clever covariates/weights for 
# the ltmle algorithm. 
# 
fitInitial <- function(data,  # data table or data frame
                       t, # numeric vector (analysis visits)
                       Anode, # character (baseline treatment variable)
                       Cnodes,# char vector (censoring variables)
                       Mnodes, # char (outcome)
                       RYnode, # char (outcome missingness)
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
                       pi=NULL
){
  
  data <- copy(data)
  K <- length(t)
  n <- nrow(data)
  
  # compute fitted value pi
  if(is.null(pi)){
    pi <- data[, mean(get(Anode)==a1)]
  }
  
  # Compute fitted values delta_k
  if(is.null(Clearner)){
    data[, paste0("pC.",1) := 1 - predict(glm(Cmodel[[1]], data=data, family="binomial"), type="response")]
    for(i in 2:K){
      set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("pC.",i), value = 
            1-predict(glm(Cmodel[[i]], data=data[data[[Cnodes[i-1]]]==0,], family="binomial"), type="response"))
    }
  }
  else{
    data[, paste0("pC.",1) := 1 - Clearner$train(make_task(data, Cmodel[[1]]))$predict()]
    for(i in 2:K){
      set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("pC.",i), value = 
            1-Clearner$train(make_task(data[data[[Cnodes[i-1]]]==0,], Cmodel[[i]]))$predict()) 
    }
  }
  
  
  # Compute fitted values p_RY
  if(is.null(RYlearner)){
    data[data[[Cnodes[K]]]==0, pRY:= predict(glm(RYmodel, data=data[data[[Cnodes[K]]]==0, ], family="binomial"), type="response")]
  }
  else{
    data[data[[Cnodes[K]]]==0, pRY:= RYlearner$train(make_task(data[data[[Cnodes[K]]]==0, ], RYmodel))$predict()]
  }

  # Compute fitted values pM and G
  nd.a1 <- copy(data)[data[[Anode]]!=a1, paste0(Anode):=paste0(a1)]
  nd.a0 <- copy(data)[data[[Anode]]!=a0, paste0(Anode):=paste0(a0)]
  
  if(is.null(Mlearner)){
    for(k in 1:K){
      data[data[[Cnodes[k]]]==0, paste0("pM.", k) := dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], 
                                                               mean=predict(fitM[[k]]), sd=sd(data[data[[Cnodes[k]]]==0][[Mnodes[k]]]))]
      data[data[[Cnodes[k]]]==0, paste0("g.a1.", k) := dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], 
                                                             mean=predict(fitg[[k]], newdata=nd.a1[nd.a1[[Cnodes[k]]]==0,]), sd=sd(data[data[[Cnodes[k]]]==0][[Mnodes[k]]]))]
      data[data[[Cnodes[k]]]==0, paste0("g.a0.", k) := dnorm(data[data[[Cnodes[k]]]==0][[Mnodes[k]]], 
                                                             mean=predict(fitg[[k]], newdata=nd.a0[nd.a0[[Cnodes[k]]]==0,]), sd=sd(data[data[[Cnodes[k]]]==0][[Mnodes[k]]]))]
    }
  }
  else{
    for(k in 1:K){
      data[data[[Cnodes[k]]]==0, paste0("pM.", k) := fitM[[k]]$predict()]
      data[data[[Cnodes[k]]]==0, paste0("g.a1.", k) := fitg[[k]]$predict(make_task(nd.a1[nd.a1[[Cnodes[k]]]==0,], gmodel[[k]]))]
      data[data[[Cnodes[k]]]==0, paste0("g.a0.", k) := fitg[[k]]$predict(make_task(nd.a0[nd.a0[[Cnodes[k]]]==0,], gmodel[[k]]))]
    }
  }
  
  # Compute weights H_k
  set(data, j=paste0("H.a1.ga1.", 1), value = (1*(data[[Anode]]==a1)/pi) * 
        (1*(data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a1.ga0.", 1), value = (1/pi) * 
        (1*(data[[Anode]]==a1 & data[[Cnodes[1]]]==0)/data[[paste0("pC.", 1)]]))
  set(data, j=paste0("H.a0.ga0.", 1), value = (1/(1-pi)) * 
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
  }
  
  set(data, j = paste0("H.a1.ga1.", K+1), value = data[[paste0("H.a1.ga1.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
          (data[[paste0("g.a1.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a1.ga0.", K+1), value = data[[paste0("H.a1.ga0.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.a0.", K)]]/data[[paste0("pM.", K)]]))
  set(data, j = paste0("H.a0.ga0.", K+1), value = data[[paste0("H.a0.ga0.", K)]] * 
        (1*(data[[RYnode]]==1)/data[["pRY"]])* 
        (data[[paste0("g.a0.", K)]]/data[[paste0("pM.", K)]]))
  
  data[is.na(data)] <- 0
  
  out_columns <-c(sapply(1:(K+1), function(i) paste0("H.a1.ga1.", i)), sapply(1:(K+1), function(i) paste0("H.a1.ga0.", i)),
                  sapply(1:(K+1), function(i) paste0("H.a0.ga0.", i)))
  data[,..out_columns]
  
}

