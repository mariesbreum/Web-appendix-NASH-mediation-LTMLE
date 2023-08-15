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
                       fitM # fitted model M
){
  
  data <- copy(data)
  K <- length(t)
  n <- nrow(data)
  
  # compute fitted value pi
  pi <- data[, mean(get(Anode)==a1)]
  
  # Compute fitted values delta_k
  if(is.null(Clearner)){
    data[, paste0("delta.",1) := 1 - predict(glm(Cmodel[[1]], data=data, family="binomial"), type="response")]
    for(i in 2:K){
      set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("delta.",i), value = data[data[[Cnodes[i-1]]]==0, ][[paste0("delta.",i-1)]]*
            (1-predict(glm(Cmodel[[i]], data=data[data[[Cnodes[i-1]]]==0,], family="binomial"), type="response"))) 
    }
  }
  else{
    data[, paste0("delta.",1) := 1 - Clearner$train(make_task(data, Cmodel[[1]]))$predict()]
    for(i in 2:K){
      set(data, i=which(data[[Cnodes[i-1]]]==0), j=paste0("delta.",i), value = data[data[[Cnodes[i-1]]]==0, ][[paste0("delta.",i-1)]]*
            (1-Clearner$train(make_task(data[data[[Cnodes[i-1]]]==0,], Cmodel[[i]]))$predict())) 
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
    data[data[[Cnodes[1]]]==0, paste0("pM.cum.", 1) := dnorm(data[data[[Cnodes[1]]]==0][[Mnodes[1]]], mean=predict(fitM[[1]]), sd=sd(data[data[[Cnodes[1]]]==0][[Mnodes[1]]]))]
  }
  else{
    data[data[[Cnodes[1]]]==0, paste0("pM.cum.", 1) := fitM[[1]]$predict()$likelihood]
  }
  
  if(is.null(glearner)){
    data[data[[Cnodes[1]]]==0, paste0("g.a1.cum.", 1) := dnorm(data[data[[Cnodes[1]]]==0][[Mnodes[1]]], mean=predict(fitg[[1]], newdata=nd.a1[nd.a1[[Cnodes[1]]]==0,]), sd=sd(data[data[[Cnodes[1]]]==0][[Mnodes[1]]]))]
    data[data[[Cnodes[1]]]==0, paste0("g.a0.cum.", 1) := dnorm(data[data[[Cnodes[1]]]==0][[Mnodes[1]]], mean=predict(fitg[[1]], newdata=nd.a0[nd.a0[[Cnodes[1]]]==0,]), sd=sd(data[data[[Cnodes[1]]]==0][[Mnodes[1]]]))]
  }
  else{
    data[data[[Cnodes[1]]]==0, paste0("g.a1.cum.", 1) := fitg[[1]]$predict(make_task(nd.a1[nd.a1[[Cnodes[1]]]==0,], gmodel[[1]]))$likelihood]
    data[data[[Cnodes[1]]]==0, paste0("g.a0.cum.", 1) := fitg[[1]]$predict(make_task(nd.a0[nd.a0[[Cnodes[1]]]==0,], gmodel[[1]]))$likelihood]
  }
  
  
  for(i in 2:K){
    if(is.null(Mlearner)){
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("pM.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]]* 
            dnorm(data[data[[Cnodes[i]]]==0][[Mnodes[i]]], mean=predict(fitM[[i]]), sd=sd(data[data[[Cnodes[i]]]==0][[Mnodes[i]]])))
    }
    else{
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("pM.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]]* 
            fitM[[i]]$predict()$likelihood)
    }
    if(is.null(glearner)){
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.a1.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.a1.cum.", i-1)]]* 
            dnorm(data[data[[Cnodes[i]]]==0][[Mnodes[i]]], mean=predict(fitg[[i]], newdata=nd.a1[nd.a1[[Cnodes[i]]]==0,]), sd=sd(data[data[[Cnodes[i]]]==0][[Mnodes[i]]])))
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.a0.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.a0.cum.", i-1)]]* 
            dnorm(data[data[[Cnodes[i]]]==0][[Mnodes[i]]], mean=predict(fitg[[i]], newdata=nd.a0[nd.a0[[Cnodes[i]]]==0,]), sd=sd(data[data[[Cnodes[i]]]==0][[Mnodes[i]]]))) 
    }
    else{
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.a1.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.a1.cum.", i-1)]]* 
            fitg[[i]]$predict(make_task(nd.a1[nd.a1[[Cnodes[i]]]==0,], gmodel[[i]]))$likelihood)
      set(data, i= which(data[[Cnodes[i]]]==0), j= paste0("g.a0.cum.", i), value = data[data[[Cnodes[i]]]==0,][[paste0("g.a0.cum.", i-1)]]* 
            fitg[[i]]$predict(make_task(nd.a0[nd.a0[[Cnodes[i]]]==0,], gmodel[[i]]))$likelihood)
    }
  }
  
  
  # Compute weights H_k
  data[, paste0("H.a1.ga1.", 1):= 0]
  set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[1]]]==0), j=paste0("H.a1.ga1.", 1), value = 1/pi * 
        1/data[data[[Anode]]==a1 & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  data[, paste0("H.a1.ga0.", 1):= 0]
  set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[1]]]==0), j=paste0("H.a1.ga0.", 1), value = 1/pi * 
        1/data[data[[Anode]]==a1 & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  data[, paste0("H.a0.ga0.", 1):= 0]
  set(data, i = which(data[[Anode]]==a0 & data[[Cnodes[1]]]==0), j=paste0("H.a0.ga0.", 1), value = 1/(1-pi) * 
        1/data[data[[Anode]]==a0 & data[[Cnodes[1]]]==0,][[paste0("delta.", 1)]])
  
  for(i in 2:K){
    data[, paste0("H.a1.ga1.", i):= 0 ]
    set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[i]]]==0), j = paste0("H.a1.ga1.", i), value = 1/pi * 
          1/data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("g.a1.cum.", i-1)]]/
          data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    data[,paste0("H.a1.ga0.",i):= 0]
    set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[i]]]==0), j = paste0("H.a1.ga0.", i), value = 1/pi * 
          1/data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("g.a0.cum.", i-1)]]/
          data[data[[Anode]]==a1 & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    data[,paste0("H.a0.ga0.", i):= 0]
    set(data, i = which(data[[Anode]]==a0 & data[[Cnodes[i]]]==0), j = paste0("H.a0.ga0.", i), value = 1/(1-pi) * 
          1/data[data[[Anode]]==a0 & data[[Cnodes[i]]]==0,][[paste0("delta.", i)]] * 
          data[data[[Anode]]==a0 & data[[Cnodes[i]]]==0,][[paste0("g.a0.cum.", i-1)]]/
          data[data[[Anode]]==a0 & data[[Cnodes[i]]]==0,][[paste0("pM.cum.", i-1)]])
    
  }
  
  data[, paste0("H.a1.ga1.", K+1):= 0]
  set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.a1.ga1.", K+1), value = 1/pi * 
        1/data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.a1.cum.", K)]]/
        data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  data[, paste0("H.a1.ga0.", K+1):= 0] 
  set(data, i = which(data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.a1.ga0.", K+1), value = 1/pi * 
        1/data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.a0.cum.", K)]]/
        data[data[[Anode]]==a1 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  data[, paste0("H.a0.ga0.", K+1):= 0]
  set(data, i = which(data[[Anode]]==a0 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1), j = paste0("H.a0.ga0.", K+1), value = 1/(1-pi) * 
        1/data[data[[Anode]]==a0 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][["pRY"]] *
        1/data[data[[Anode]]==a0 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("delta.", K)]] *
        data[data[[Anode]]==a0 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("g.a0.cum.", K)]]/
        data[data[[Anode]]==a0 & data[[Cnodes[K]]]==0 & data[[RYnode]]==1,][[paste0("pM.cum.", K)]]) 
  
  out_columns <-c(sapply(1:(K+1), function(i) paste0("H.a1.ga1.", i)), sapply(1:(K+1), function(i) paste0("H.a1.ga0.", i)),
                  sapply(1:(K+1), function(i) paste0("H.a0.ga0.", i)))
  data[,..out_columns]
  
}

