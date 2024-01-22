# -----------------------------------------------------------------------------
# fitLTMLE
# -----------------------------------------------------------------------------
# Description:
# This function returns targeted estimates of the direct and indirect effects. 
#
fitLTMLE <- function(data, # data table or data frame 
                     L0nodes, # character vector (baseline covariates)
                     Anode, # character (baseline treatment variable)
                     Cnodes, # char vector (censoring variables)
                     Lnodes, # char vector (time-varying covariates)
                     Mnodes, # char vector (mediator variables)
                     RYnode, # char (outcome missingness)
                     Ynode, # char (outcome)
                     Cmodel, # list of models for C_i
                     Mmodel, # list of models for M_i
                     gmodel, # list of models for g_i
                     RYmodel, # model for R.Y
                     Ymodel, # model for initial regression QY
                     QLmodel, # model for initial regression QL
                     a1, # level of treatment var corresponding to a1
                     a0, # level of treatment var corresponding to a0
                     pi = NULL, # if NULL the propensity score is estimated from the data
                     n_bins = 50, # number of partitions used for the numerical integration
                     Ylearner = NULL,
                     RYlearner = NULL,
                     Mlearner = NULL,
                     glearner = NULL,
                     QLlearner = NULL,
                     Clearner = NULL,
                     alpha = 0.00001 # truncation (alpha, 1-alpha) used to bound Q_Y away from 1 and 0
                     
){
  data.table::setDT(data)
  cols <- c(L0nodes, Anode, Cnodes, Lnodes, Mnodes, RYnode, Ynode)
  data <- data.table::copy(data)[,..cols]
  
  # set-up 
  K <- length(Mnodes)
  n <- nrow(data)
  
  if(is.null(pi)){
    pi <- data[, mean(get(Anode)==a1)]
  }
  
  if(is.null(QLlearner)){
    QLlearner <- sl3::Lrnr_glm$new(family=quasibinomial())
  }

  suppressMessages({
  # fit g model
  fitg <- list()
  if(is.null(glearner)){
    for(i in 1:K){
      fitg[[i]] <- lm(gmodel[[i]], data=data[data[[Cnodes[i]]]==0,])
    }
  }
  else{
    for(i in 1:K){
      fitg[[i]] <- glearner$train(make_task(data[data[[Cnodes[i]]]==0,], gmodel[[i]]))
    }
  }
  
  # fit M model
  if(identical(gmodel, Mmodel) == TRUE & identical(glearner, Mlearner) == TRUE){
    fitM <- fitg
  }
  else{
    fitM <- list()
    if(is.null(Mlearner)){
      for(i in 1:K){
        fitM[[i]] <- lm(Mmodel[[i]], data=data[data[[Cnodes[i]]]==0,])
      }
    }
    else{
      for(i in 1:K){
        fitM[[i]] <- Mlearner$train(make_task(data[data[[Cnodes[i]]]==0,], Mmodel[[i]]))
      }
    }
  }
  })
  
  # compute weights/clever covariates
  data <- cbind(data, fitInitial(data, Anode, Cnodes, Mnodes, RYnode, Cmodel, gmodel, RYmodel, a1, a0,
                                 Clearner, RYlearner, glearner, Mlearner, fitg, fitM, pi))


  # fit initial Y model
  if(is.null(Ylearner)){
    fitY <- glm(Ymodel, data = data[data[[RYnode]]==1,], family="binomial")
  }
  else{
    fitY <- Ylearner$train(make_task(data[data[[RYnode]]==1,], Ymodel))
  }
  
  ### Compute QY_star ###
  
  # initial predictions QY
  nd.a1 <- copy(data)[data[[Anode]]!=a1, paste0(Anode):=paste0(a1)]
  nd.a0 <- copy(data)[data[[Anode]]!=a0, paste0(Anode):=paste0(a0)]

  if(is.null(Ylearner)){
    QY.a1 <- pmin(1-alpha, pmax(alpha, predict(fitY, newdata=nd.a1[nd.a1[[Cnodes[K]]]==0], type="response")))
    QY.a0 <- pmin(1-alpha, pmax(alpha, predict(fitY, newdata=nd.a0[nd.a0[[Cnodes[K]]]==0], type="response")))
  }
  else{
    QY.a1 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a1[nd.a1[[Cnodes[K]]]==0], Ymodel))))
    QY.a0 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a0[nd.a0[[Cnodes[K]]]==0], Ymodel))))
  }
  
  # target QY
  eps.a1.ga1 <- coef(glm.fit(x=rep(1, nrow(data[data[[Cnodes[K]]]==0,])), y = data[data[[Cnodes[K]]]==0,][[Ynode]], 
                             weights = data[data[[Cnodes[K]]]==0,][[paste0("H.a1.ga1.", K+1)]], offset = qlogis(QY.a1), 
                             family = quasibinomial()))
  eps.a1.ga0 <- coef(glm.fit(x=rep(1, nrow(data[data[[Cnodes[K]]]==0,])), y = data[data[[Cnodes[K]]]==0,][[Ynode]], 
                             weights = data[data[[Cnodes[K]]]==0,][[paste0("H.a1.ga0.", K+1)]], offset = qlogis(QY.a1), 
                             family = quasibinomial()))
  eps.a0.ga0 <- coef(glm.fit(x=rep(1, nrow(data[data[[Cnodes[K]]]==0,])), y = data[data[[Cnodes[K]]]==0,][[Ynode]], 
                             weights = data[data[[Cnodes[K]]]==0,][[paste0("H.a0.ga0.", K+1)]], offset = qlogis(QY.a0), 
                             family = quasibinomial()))
  
  # Update QY
  data[, "QY_star.a1.ga1" := 0]
  data[, "QY_star.a1.ga0" := 0]
  data[, "QY_star.a0.ga0" := 0]
  data[data[[Cnodes[K]]]==0 , "QY_star.a1.ga1" := plogis(qlogis(QY.a1) + eps.a1.ga1)]
  data[data[[Cnodes[K]]]==0 , "QY_star.a1.ga0" := plogis(qlogis(QY.a1) + eps.a1.ga0)]
  data[data[[Cnodes[K]]]==0 , "QY_star.a0.ga0" := plogis(qlogis(QY.a0) + eps.a0.ga0)]

  ### Compute QM_K ###
  
  # discrete grid m_K
  m_K_discrete <- sl3:::make_bins(data[data[[Cnodes[K]]]==0, ][[Mnodes[K]]], type = "equal_range", n_bins = n_bins)
  
  # integrate out m_K
  data[, paste0("QM_", K, ".a1.ga1") := 0]
  data[, paste0("QM_", K, ".a1.ga0") := 0]
  data[, paste0("QM_", K, ".a0.ga0") := 0]

  for(i in 1:(n_bins-1)){
    nd.a1.mk <- copy(nd.a1)[nd.a1[[Mnodes[K]]]!=m_K_discrete[i], Mnodes[K]:=m_K_discrete[i]]
    nd.a0.mk <- copy(nd.a0)[nd.a0[[Mnodes[K]]]!=m_K_discrete[i], Mnodes[K]:=m_K_discrete[i]]
    
    # predict QY
    if(is.null(Ylearner)){
      QY.a1 <- pmin(1-alpha, pmax(alpha, predict(fitY, newdata=nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,], type="response")))
      QY.a0 <- pmin(1-alpha, pmax(alpha, predict(fitY, newdata=nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,], type="response")))
    }
    else{
      QY.a1 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,], Ymodel))))
      QY.a0 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,], Ymodel))))
    }
    # predict g_a_K
    if(is.null(glearner)){
      mu.g.a1 <- predict(fitg[[K]], newdata=nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,])
      g.a1 <- dnorm(m_K_discrete[i], mean = mu.g.a1, sd=sd(fitg[[K]]$residuals))
      mu.g.a0 <- predict(fitg[[K]], newdata=nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,])
      g.a0 <- dnorm(m_K_discrete[i], mean = mu.g.a0, sd=sd(fitg[[K]]$residuals))
    }
    else{
      g.a1 <- fitg[[K]]$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))$likelihood
      g.a0 <- fitg[[K]]$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))$likelihood
    }
    set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a1.ga1"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a1.ga1")]] + 
          plogis(qlogis(QY.a1) + eps.a1.ga1)*g.a1*diff(m_K_discrete)[i])
    set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a1.ga0"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a1.ga0")]] + 
          plogis(qlogis(QY.a1) + eps.a1.ga0)*g.a0*diff(m_K_discrete)[i])
    set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a0.ga0"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a0.ga0")]] + 
          plogis(qlogis(QY.a0) + eps.a0.ga0)*g.a0*diff(m_K_discrete)[i])
  }
  
  QLcov <- lapply(1:K, function(i) paste(all.vars(as.formula(QLmodel[[i]]))[2:length(all.vars(as.formula(QLmodel[[i]])))], collapse="+"))
  
  for(k in 1:(K-1)){
    
    ### Compute QL_k+1_star ###
    
    # fit initial QL_k+1
    fitL.a1.ga1 <- QLlearner$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a1.ga1~",QLcov[K-k+1])))
    fitL.a1.ga0 <- QLlearner$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a1.ga0~",QLcov[K-k+1])))
    fitL.a0.ga0 <- QLlearner$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a0.ga0~",QLcov[K-k+1])))
    
    # initial predictions QL_k+1
    newcols <- c(paste0("QM_",K-k+1, ".a1.ga1"), paste0("QM_",K-k+1, ".a1.ga0"), paste0("QM_",K-k+1, ".a0.ga0"))
    nd.a1 <- cbind(nd.a1, data[,..newcols])
    nd.a0 <- cbind(nd.a0, data[,..newcols])
    
    QL.a1.ga1 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga1$predict(make_prediction_task(nd.a1[nd.a1[[Cnodes[K-k]]]==0,], 
                                                                         paste0("QM_",K-k+1, ".a1.ga1", "~",QLcov[K-k+1])))))
    QL.a1.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga0$predict(make_prediction_task(nd.a1[nd.a1[[Cnodes[K-k]]]==0,], 
                                                                         paste0("QM_",K-k+1, ".a1.ga0", "~",QLcov[K-k+1])))))
    QL.a0.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a0.ga0$predict(make_prediction_task(nd.a0[nd.a0[[Cnodes[K-k]]]==0,],
                                                                         paste0("QM_",K-k+1, ".a0.ga0", "~",QLcov[K-k+1])))))

    # target QL_k+1
    eps.a1.ga1 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k]]]==0,])), y = data[data[[Cnodes[K-k]]]==0,][[paste0("QM_",K-k+1, ".a1.ga1")]], 
                               weights = data[data[[Cnodes[K-k]]]==0,][[paste0("H.a1.ga1.", K-k+1)]], offset = qlogis(QL.a1.ga1), 
                               family = quasibinomial()))
      
    eps.a1.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k]]]==0,])), y = data[data[[Cnodes[K-k]]]==0,][[paste0("QM_",K-k+1, ".a1.ga0")]], 
                               weights = data[data[[Cnodes[K-k]]]==0,][[paste0("H.a1.ga0.", K-k+1)]], offset = qlogis(QL.a1.ga0), 
                               family = quasibinomial()))
    
    eps.a0.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k]]]==0,])), y = data[data[[Cnodes[K-k]]]==0,][[paste0("QM_",K-k+1, ".a0.ga0")]], 
                               weights = data[data[[Cnodes[K-k]]]==0,][[paste0("H.a0.ga0.",K-k+1)]], offset = qlogis(QL.a0.ga0), 
                               family = quasibinomial()))
    
    
    data[, paste0("QL_", K-k+1, ".a1.ga1.star"):= 0]
    data[, paste0("QL_", K-k+1, ".a1.ga0.star"):= 0]
    data[, paste0("QL_", K-k+1, ".a0.ga0.star"):= 0]
    data[data[[Cnodes[K-k]]]==0, paste0("QL_", K-k+1, ".a1.ga1.star"):= plogis(qlogis(QL.a1.ga1) + eps.a1.ga1)]
    data[data[[Cnodes[K-k]]]==0, paste0("QL_", K-k+1, ".a1.ga0.star"):= plogis(qlogis(QL.a1.ga0) + eps.a1.ga0)]
    data[data[[Cnodes[K-k]]]==0, paste0("QL_", K-k+1, ".a0.ga0.star"):= plogis(qlogis(QL.a0.ga0) + eps.a0.ga0)]

    
    ### Compute QM_k ###
    
    # discrete grid m_k
    m_k_discrete <- sl3:::make_bins(data[data[[Cnodes[K-k]]]==0, ][[Mnodes[K-k]]], type="equal_range", n_bins=n_bins)
    
    # integrate out m_k
    data[, paste0("QM_", K-k, ".a1.ga1") :=0]
    data[, paste0("QM_", K-k, ".a1.ga0") :=0]
    data[, paste0("QM_", K-k, ".a0.ga0") :=0]
    
    for(i in 1:(n_bins-1)){
      nd.a1.mk <- copy(nd.a1)[nd.a1[[Mnodes[K-k]]]!= m_k_discrete[i], Mnodes[K-k]:=m_k_discrete[i]]
      nd.a0.mk <- copy(nd.a0)[nd.a0[[Mnodes[K-k]]]!= m_k_discrete[i],Mnodes[K-k]:=m_k_discrete[i]]
      
      # predictions QL_k+1
      QL.a1.ga1 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga1$predict(make_prediction_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], 
                                                                    paste0("QM_",K-k+1, ".a1.ga1", "~",QLcov[K-k+1])))))
      QL.a1.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga0$predict(make_prediction_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], 
                                                                    paste0("QM_",K-k+1, ".a1.ga0", "~",QLcov[K-k+1])))))
      QL.a0.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a0.ga0$predict(make_prediction_task(nd.a0.mk[nd.a0.mk[[Cnodes[K-k]]]==0,],
                                                                    paste0("QM_",K-k+1, ".a0.ga0", "~",QLcov[K-k+1])))))


      # predict g_a_k
      if(is.null(glearner)){
        mu.g.a1 <- predict(fitg[[K-k]], newdata=nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,])
        g.a1 <- dnorm(m_k_discrete[i], mean = mu.g.a1, sd=sd(fitg[[k]]$residuals))
        mu.g.a0 <- predict(fitg[[K-k]], newdata=nd.a0.mk[nd.a0.mk[[Cnodes[K-k]]]==0,])
        g.a0 <- dnorm(m_k_discrete[i], mean = mu.g.a0, sd=sd(fitg[[k]]$residuals))
      }
      else{
        g.a1 <- fitg[[K-k]]$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))$likelihood
        g.a0 <- fitg[[K-k]]$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))$likelihood
      }
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a1.ga1"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a1.ga1")]] + 
            plogis(qlogis(QL.a1.ga1) + eps.a1.ga1)*g.a1*diff(m_k_discrete)[i])
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a1.ga0"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a1.ga0")]] + 
            plogis(qlogis(QL.a1.ga0) + eps.a1.ga0)*g.a0*diff(m_k_discrete)[i])
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a0.ga0"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a0.ga0")]] + 
            plogis(qlogis(QL.a0.ga0) + eps.a0.ga0)*g.a0*diff(m_k_discrete)[i])
      
    }
  }
  
  ### Compute QL_1_star ###
  
  # fit initial QL_1
  fitL.a1.ga1 <- QLlearner$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a1.ga1", "~",QLcov[1])))
  fitL.a1.ga0 <- QLlearner$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a1.ga0", "~",QLcov[1])))
  fitL.a0.ga0 <- QLlearner$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a0.ga0", "~",QLcov[1])))

  # initial predictions QL_1
  newcols <- c("QM_1.a1.ga1", "QM_1.a1.ga0", "QM_1.a0.ga0")
  nd.a1 <- cbind(nd.a1, data[,..newcols])
  nd.a0 <- cbind(nd.a0, data[,..newcols])
  QL1.a1.ga1 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga1$predict(make_prediction_task(nd.a1, 
                                                                 paste0("QM_1.a1.ga1", "~",QLcov[1])))))
  QL1.a1.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a1.ga0$predict(make_prediction_task(nd.a1, 
                                                                 paste0("QM_1.a1.ga0", "~",QLcov[1])))))
  QL1.a0.ga0 <- pmin(1-alpha, pmax(alpha, fitL.a0.ga0$predict(make_prediction_task(nd.a0, 
                                                                 paste0("QM_1.a0.ga0", "~",QLcov[1])))))


  # target QL_1
  eps.a1.ga1 <- coef(glm.fit(x = rep(1, n), y = data[["QM_1.a1.ga1"]], 
                             weights = data[[paste0("H.a1.ga1.", 1)]], offset = qlogis(QL1.a1.ga1), 
                             family = quasibinomial()))
  eps.a1.ga0 <- coef(glm.fit(x = rep(1, n), y = data[["QM_1.a1.ga0"]], 
                             weights = data[[paste0("H.a1.ga0.", 1)]], offset = qlogis(QL1.a1.ga0), 
                             family = quasibinomial()))
  eps.a0.ga0 <- coef(glm.fit(x = rep(1, n), y = data[["QM_1.a0.ga0"]], 
                             weights = data[[paste0("H.a0.ga0.", 1)]], offset = qlogis(QL1.a0.ga0), 
                             family = quasibinomial()))

  set(data, j="QL_1.a1.ga1.star", value= plogis(qlogis(QL1.a1.ga1) + eps.a1.ga1))
  set(data, j="QL_1.a1.ga0.star", value= plogis(qlogis(QL1.a1.ga0) + eps.a1.ga0)) 
  set(data, j="QL_1.a0.ga0.star", value= plogis(qlogis(QL1.a0.ga0) + eps.a0.ga0))


  ### Compute eif ###
  set(data, j="eif.a1.ga1", value=data[[paste0("H.a1.ga1.", K+1)]]*(data[[Ynode]]-data[["QY_star.a1.ga1"]])+
    (data[["QL_1.a1.ga1.star"]]-data[, mean(QL_1.a1.ga1.star)]))
  set(data, j="eif.a1.ga0", value=data[[paste0("H.a1.ga0.", K+1)]]*(data[[Ynode]]-data[["QY_star.a1.ga0"]])+
        (data[["QL_1.a1.ga0.star"]]-data[, mean(QL_1.a1.ga0.star)]))
  set(data, j="eif.a0.ga0", value=data[[paste0("H.a0.ga0.", K+1)]]*(data[[Ynode]]-data[["QY_star.a0.ga0"]])+
        (data[["QL_1.a0.ga0.star"]]-data[, mean(QL_1.a0.ga0.star)]))

  
  for(i in 1:K){
    set(data, j="eif.a1.ga1", value = data[["eif.a1.ga1"]] + 
          data[[paste0("H.a1.ga1.", i)]]*(data[[paste0("QM_", i, ".a1.ga1")]] - data[[paste0("QL_", i, ".a1.ga1.star")]]))
    set(data, j="eif.a1.ga0", value = data[["eif.a1.ga0"]] + 
          data[[paste0("H.a1.ga0.", i)]]*(data[[paste0("QM_", i, ".a1.ga0")]] - data[[paste0("QL_", i, ".a1.ga0.star")]]))
    set(data, j="eif.a0.ga0", value = data[["eif.a0.ga0"]] + 
          data[[paste0("H.a0.ga0.", i)]]*(data[[paste0("QM_", i, ".a0.ga0")]] - data[[paste0("QL_", i, ".a0.ga0.star")]]))
  }
  
  ### Estimands ###
  psi.a1.ga1 <-data[, mean(QL_1.a1.ga1.star)]
  psi.a1.ga0 <-data[, mean(QL_1.a1.ga0.star)]
  psi.a0.ga0 <-data[, mean(QL_1.a0.ga0.star)]

  est.psi <- data.frame(est.psi11=psi.a1.ga1, est.psi10=psi.a1.ga0, est.psi00=psi.a0.ga0, 
                        se.psi11=data[, sd(eif.a1.ga1)/sqrt(n)], se.psi10=data[, sd(eif.a1.ga0)/sqrt(n)], 
                        se.psi00=data[, sd(eif.a0.ga0)/sqrt(n)])
    
  est.diff <- data.frame(est.sde=psi.a1.ga0-psi.a0.ga0, est.sie=psi.a1.ga1-psi.a1.ga0, est.oe=psi.a1.ga1-psi.a0.ga0, 
                         est.pm=(psi.a1.ga1-psi.a1.ga0)/(psi.a1.ga1-psi.a0.ga0),
                         se.sde=data[, sd(eif.a1.ga0 - eif.a0.ga0)/sqrt(n)], se.sie=data[, sd(eif.a1.ga1 - eif.a1.ga0)/sqrt(n)], 
                         se.oe=data[, sd(eif.a1.ga1 - eif.a0.ga0)/sqrt(n)], 
                         se.pm=data[, sd(((eif.a1.ga1 - eif.a1.ga0)*(psi.a1.ga1-psi.a0.ga0) - 
                                            (psi.a1.ga1-psi.a1.ga0)*(eif.a1.ga1 - eif.a0.ga0))/(psi.a1.ga1-psi.a0.ga0)^2)/sqrt(n)])
  
  est.OR <- data.frame(est.ORsde = (psi.a1.ga0/(1-psi.a1.ga0)) / (psi.a0.ga0/(1-psi.a0.ga0)), 
                       est.ORsie = (psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a1.ga0/(1-psi.a1.ga0)), 
                       est.ORoe = (psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a0.ga0/(1-psi.a0.ga0)), 
                       se.ORsde = data[, sd((1-psi.a0.ga0)/((1-psi.a1.ga0)^2*psi.a0.ga0)*eif.a1.ga0 -
                                             psi.a1.ga0/((1-psi.a1.ga0)*psi.a0.ga0^2)*eif.a0.ga0)/sqrt(n)], 
                       se.ORsie=data[, sd((1-psi.a1.ga0)/((1-psi.a1.ga1)^2*psi.a1.ga0)*eif.a1.ga1 -
                                            psi.a1.ga1/((1-psi.a1.ga1)*psi.a1.ga0^2)*eif.a1.ga0)/sqrt(n)], 
                       se.ORoe=data[, sd((1-psi.a0.ga0)/((1-psi.a1.ga1)^2*psi.a0.ga0)*eif.a1.ga1 -
                                           psi.a1.ga1/((1-psi.a1.ga1)*psi.a0.ga0^2)*eif.a0.ga0)/sqrt(n)])
  
  
  est.logOR <- data.frame(est.logORsde = log((psi.a1.ga0/(1-psi.a1.ga0)) / (psi.a0.ga0/(1-psi.a0.ga0))), 
                          est.logORsie = log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a1.ga0/(1-psi.a1.ga0))), 
                          est.logORoe = log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a0.ga0/(1-psi.a0.ga0))), 
                          est.logORpm = log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a1.ga0/(1-psi.a1.ga0)))/
                            log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a0.ga0/(1-psi.a0.ga0))),
                          se.logORsde = data[, sd(eif.a1.ga0/(psi.a1.ga0*(1-psi.a1.ga0))-
                                                 eif.a0.ga0/(psi.a0.ga0*(1-psi.a0.ga0)))/sqrt(n)], 
                          se.logORsie=data[, sd(eif.a1.ga1/(psi.a1.ga1*(1-psi.a1.ga1))-
                                               eif.a1.ga0/(psi.a1.ga0*(1-psi.a1.ga0)))/sqrt(n)], 
                          se.logORoe=data[, sd(eif.a1.ga1/(psi.a1.ga1*(1-psi.a1.ga1))-
                                              eif.a0.ga0/(psi.a0.ga0*(1-psi.a0.ga0)))/sqrt(n)],
                          se.logORpm=data[, sd((eif.a1.ga0/(psi.a1.ga0*(1-psi.a1.ga0))-eif.a0.ga0/(psi.a0.ga0*(1-psi.a0.ga0)))/
                                                 log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a0.ga0/(1-psi.a0.ga0))) -
                                                 ((eif.a1.ga1/(psi.a1.ga1*(1-psi.a1.ga1))-eif.a0.ga0/(psi.a0.ga0*(1-psi.a0.ga0)))*
                                                    log((psi.a1.ga0/(1-psi.a1.ga0)) / (psi.a0.ga0/(1-psi.a0.ga0))))/
                                                 (log((psi.a1.ga1/(1-psi.a1.ga1)) / (psi.a0.ga0/(1-psi.a0.ga0))))^2)/sqrt(n)])
  
  est.all <- cbind(est.diff, est.psi, est.OR, est.logOR)
  
  if(is.null(glearner)){
    out <- list(est.diff=est.diff, est.OR=est.OR, est.psi=est.psi, est.logOR=est.logOR, est.all=est.all, fitg=fitg, pi=pi) 
  }
  else{
    out <- list(est.diff=est.diff, est.OR=est.OR, est.psi=est.psi, est.logOR=est.logOR, est.all=est.all, pi=pi) 
  }
  
  class(out) <- "fitLTMLE"
  
  return(out)
}  


