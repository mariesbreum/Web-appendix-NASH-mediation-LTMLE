# -----------------------------------------------------------------------------
# Functions - LTMLE 
# -----------------------------------------------------------------------------
# Description:
# This function 

fitLTMLE_lin <- function(data, # data table or data frame 
                     t, # numeric vector (analysis visits)
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
                     QLmodel, # list of covariates for regressions QL_i
                     a1, # level of treatment var corresponding to a1
                     a0, # level of treatment var corresponding to a0
                     n_bins, # number of partitions used for the numerical integration
                     Ylearner = NULL,
                     RYlearner = NULL,
                     Mlearner = NULL,
                     glearner = NULL,
                     QLlearner = NULL,
                     Clearner = NULL,
                     alpha = 0.005 # truncation (alpha, 1-alpha) used to bound Q_Y away from 1 and 0
                     
){
  data.table::setDT(data)
  cols <- c(L0nodes, Anode, Cnodes, Lnodes, Mnodes, RYnode, Ynode)
  data <- data.table::copy(data)[,..cols]
  
  # set-up 
  K <- length(t)
  n <- nrow(data)
  
  # Learners
  if(is.null(Ylearner)){
    lrn_Y <- sl3::Lrnr_glm_fast$new()
  }
  else{
    lrn_Y <- Ylearner
  }
  
  if(is.null(QLlearner)){
    stack <- sl3::Stack$new(Lrnr_glm_fast$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                            Lrnr_gam$new(), Lrnr_caret$new(algorithm  = "glmStepAIC", trace=F))
    corP_screen <- sl3::Lrnr_screener_correlation$new(type = "threshold", pvalue_threshold = 0.05)
    lrn_QL <- sl3::Lrnr_sl$new(learners = Stack$new(stack, Pipeline$new(corP_screen, stack)))
  }
  else{
    lrn_QL <- QLlearner
  }
  
  if(is.null(Mlearner)){
    #lrn_M <- sl3::Lrnr_density_semiparametric$new(mean_learner = Lrnr_glm_fast$new())
    lrn_M <-Lrnr_density_normal$new()
  }
  else{
    lrn_M <- Mlearner
  }
  
  if(is.null(glearner)){
    lrn_g <- lrn_M
  }
  else{
    lrn_g <- glearner
  }
  
  # fit g model
  fitg <- list()
  for(i in 1:K){
    fitg[[i]] <- lrn_g$train(make_task(data[data[[Cnodes[i]]]==0,], gmodel[[i]]))
  }
  
  # fit M model
  if(identical(lrn_g, lrn_M) == TRUE & identical(gmodel, Mmodel) == TRUE){
    fitM <- fitg
  }
  else{
    fitM <- list()
    for(i in 1:K){
      fitM[[i]] <- lrn_M$train(make_task(data[data[[Cnodes[i]]]==0,], Mmodel[[i]]))
    }
  }
  
  # compute weights/clever covariates
  data <- cbind(data, fitInitial(data, t, Anode, Cnodes, RYnode, Cmodel, gmodel, RYmodel, a1, a0,
                                 Clearner, RYlearner, fitg, fitM))
  
  # fit initial Y model
  fitY <- lrn_Y$train(make_task(data[data[[RYnode]]==1,], Ymodel))
  
  ### Compute QY_star ###
  
  # initial predictions QY
  nd.a1 <- copy(data)[, paste0(Anode):=rep(a1, n)]
  nd.a0 <- copy(data)[, paste0(Anode):=rep(a0, n)]
  QY.a1 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a1[nd.a1[[RYnode]]==1,], Ymodel))))
  QY.a0 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_prediction_task(nd.a0[nd.a0[[RYnode]]==1,], Ymodel))))
  
  # target QY
  eps.a1.ga1 <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                             weights = data[data[[RYnode]]==1,][[paste0("H.a1.ga1.", K+1)]], offset = qlogis(QY.a1), 
                             family = quasibinomial()))
  eps.a1.ga0 <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                             weights = data[data[[RYnode]]==1,][[paste0("H.a1.ga0.", K+1)]], offset = qlogis(QY.a1), 
                             family = quasibinomial()))
  eps.a0.ga0 <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                             weights = data[data[[RYnode]]==1,][[paste0("H.a0.ga0.", K+1)]], offset = qlogis(QY.a0), 
                             family = quasibinomial()))
  
  # Update QY
  data[data[[RYnode]]==1, "QY_star.a1.ga1" := plogis(qlogis(QY.a1) + eps.a1.ga1)]
  data[data[[RYnode]]==1, "QY_star.a1.ga0" := plogis(qlogis(QY.a1) + eps.a1.ga0)]
  data[data[[RYnode]]==1, "QY_star.a0.ga0" := plogis(qlogis(QY.a0) + eps.a0.ga0)]
  
  ### Compute QM_K ###
  
  # discrete grid m_K
  m_K_discrete <- sl3:::make_bins(data[data[[Cnodes[K]]]==0, ][[Mnodes[K]]], type = "equal_range", n_bins = n_bins)
  
  # integrate out m_K
  data[, paste0("QM_", K, ".a1.ga1") := 0]
  data[, paste0("QM_", K, ".a1.ga0") := 0]
  data[, paste0("QM_", K, ".a0.ga0") := 0]
  for(i in 1:n_bins){
    nd.a1.mk <- copy(nd.a1)[,Mnodes[K]:=m_K_discrete[i]]
    nd.a0.mk <- copy(nd.a0)[,Mnodes[K]:=m_K_discrete[i]]
    
    # predict QY
    QY.a1 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,], Ymodel))))
    QY.a0 <- pmin(1-alpha, pmax(alpha, fitY$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,], Ymodel))))
    
    # predict g_a_K
    g.a1 <- fitg[[K]]$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))
    g.a0 <- fitg[[K]]$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))
    
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
    fitL.a1.ga1 <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a1.ga1~",QLcov[K-k+1])))
    fitL.a1.ga0 <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a1.ga0~",QLcov[K-k+1])))
    fitL.a0.ga0 <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a0.ga0~",QLcov[K-k+1])))
    
    # initial predictions QL_k+1
    newcols <- c(paste0("QM_",K-k+1, ".a1.ga1"), paste0("QM_",K-k+1, ".a1.ga0"), paste0("QM_",K-k+1, ".a0.ga0"))
    nd.a1 <- cbind(nd.a1, data[,..newcols])
    nd.a0 <- cbind(nd.a0, data[,..newcols])
    
    QL.a1.ga1 <- fitL.a1.ga1$predict(make_task(nd.a1[nd.a1[[Cnodes[K-k+1]]]==0,], 
                                     paste0("QM_",K-k+1, ".a1.ga1", "~",QLcov[K-k+1])))
    QL.a1.ga0 <- fitL.a1.ga0$predict(make_task(nd.a1[nd.a1[[Cnodes[K-k+1]]]==0,], 
                                     paste0("QM_",K-k+1, ".a1.ga0", "~",QLcov[K-k+1])))
    QL.a0.ga0 <- fitL.a0.ga0$predict(make_task(nd.a0[nd.a0[[Cnodes[K-k+1]]]==0,],
                                     paste0("QM_",K-k+1, ".a0.ga0", "~",QLcov[K-k+1])))
    
    # target QL_k+1
    eps.a1.ga1 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".a1.ga1")]], 
                               weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.a1.ga1.", K-k+1)]], offset = QL.a1.ga1))
    
    eps.a1.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".a1.ga0")]], 
                               weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.a1.ga0.", K-k+1)]], offset = QL.a1.ga0))
    
    eps.a0.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".a0.ga0")]], 
                               weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.a0.ga0.",K-k+1)]], offset = QL.a0.ga0))
    
    data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".a1.ga1.star"):= QL.a1.ga1 + eps.a1.ga1]
    data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".a1.ga0.star"):= QL.a1.ga0 + eps.a1.ga0]
    data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".a0.ga0.star"):= QL.a0.ga0 + eps.a0.ga0]
    
    ### Compute QM_k ###
    
    # discrete grid m_k
    m_k_discrete <- sl3:::make_bins(data[data[[Cnodes[K-k]]]==0, ][[Mnodes[K-k]]], type="equal_range", n_bins=n_bins)
    
    # integrate out m_k
    data[, paste0("QM_", K-k, ".a1.ga1") :=0]
    data[, paste0("QM_", K-k, ".a1.ga0") :=0]
    data[, paste0("QM_", K-k, ".a0.ga0") :=0]
    
    for(i in 1:n_bins){
      nd.a1.mk <- copy(nd.a1)[,Mnodes[K-k]:=m_k_discrete[i]]
      nd.a0.mk <- copy(nd.a0)[,Mnodes[K-k]:=m_k_discrete[i]]
      
      # predictions QL_k+1
      QL.a1.ga1 <- fitL.a1.ga1$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], 
                                       paste0("QM_",K-k+1, ".a1.ga1", "~",QLcov[K-k+1])))
      QL.a1.ga0 <- fitL.a1.ga0$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], 
                                       paste0("QM_",K-k+1, ".a1.ga0", "~",QLcov[K-k+1])))
      QL.a0.ga0 <- fitL.a0.ga0$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K-k]]]==0,],
                                       paste0("QM_",K-k+1, ".a0.ga0", "~",QLcov[K-k+1])))
      
      # predict g_a_k
      g.a1 <- fitg[[K-k]]$predict(make_task(nd.a1.mk[nd.a1.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))
      g.a0 <- fitg[[K-k]]$predict(make_task(nd.a0.mk[nd.a0.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))
      
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a1.ga1"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a1.ga1")]] + 
            (QL.a1.ga1+ eps.a1.ga1)*g.a1*diff(m_k_discrete)[i])
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a1.ga0"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a1.ga0")]] + 
            (QL.a1.ga0 + eps.a1.ga0)*g.a0*diff(m_k_discrete)[i])
      set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a0.ga0"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a0.ga0")]] + 
            (QL.a0.ga0 + eps.a0.ga0)*g.a0*diff(m_k_discrete)[i])
      
    }
    
  }
  
  ### Compute QL_1_star ###
  
  # fit initial QL_1
  fitL.a1.ga1 <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a1.ga1", "~",QLcov[1])))
  fitL.a1.ga0 <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a1.ga0", "~",QLcov[1])))
  fitL.a0.ga0 <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a0.ga0", "~",QLcov[1])))
  
  # initial predictions QL_1
  newcols <- c("QM_1.a1.ga1", "QM_1.a1.ga0", "QM_1.a0.ga0")
  nd.a1 <- cbind(nd.a1, data[,..newcols])
  nd.a0 <- cbind(nd.a0, data[,..newcols])
  QL1.a1.ga1 <- fitL.a1.ga1$predict(make_task(nd.a1[nd.a1[[Cnodes[1]]]==0,], 
                                    paste0("QM_1.a1.ga1", "~",QLcov[1])))
  QL1.a1.ga0 <- fitL.a1.ga0$predict(make_task(nd.a1[nd.a1[[Cnodes[1]]]==0,], 
                                    paste0("QM_1.a1.ga0", "~",QLcov[1])))
  QL1.a0.ga0 <- fitL.a0.ga0$predict(make_task(nd.a0[nd.a0[[Cnodes[1]]]==0,], 
                                    paste0("QM_1.a0.ga0", "~",QLcov[1])))
  
  # target QL_1
  eps.a1.ga1 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.a1.ga1"]], 
                             weights = data[data[[Cnodes[1]]]==0,][[paste0("H.a1.ga1.", 1)]], offset = QL1.a1.ga1))
  eps.a1.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.a1.ga0"]], 
                             weights = data[data[[Cnodes[1]]]==0,][[paste0("H.a1.ga0.", 1)]], offset = QL1.a1.ga0))
  eps.a0.ga0 <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.a0.ga0"]], 
                             weights = data[data[[Cnodes[1]]]==0,][[paste0("H.a0.ga0.", 1)]], offset = QL1.a0.ga0))
  
  set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.a1.ga1.star", value= QL1.a1.ga1 + eps.a1.ga1)
  set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.a1.ga0.star", value= QL1.a1.ga0 + eps.a1.ga0) 
  set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.a0.ga0.star", value= QL1.a0.ga0 + eps.a0.ga0)
  
  ### Compute eif ###
  set(data, j="eif.a1.ga1", value=data[[paste0("H.a1.ga1.", K+1)]]*(data[[Ynode]]-data[["QY_star.a1.ga1"]]))
  set(data, j="eif.a1.ga0", value=data[[paste0("H.a1.ga0.", K+1)]]*(data[[Ynode]]-data[["QY_star.a1.ga0"]]))
  set(data, j="eif.a0.ga0", value=data[[paste0("H.a0.ga0.", K+1)]]*(data[[Ynode]]-data[["QY_star.a0.ga0"]]))
  
  for(i in 1:K){
    set(data, j="eif.a1.ga1", value = data[["eif.a1.ga1"]] + 
          data[[paste0("H.a1.ga1.", i)]]*(data[[paste0("QM_", i, ".a1.ga1")]] - data[[paste0("QL_", i, ".a1.ga1.star")]]))
    set(data, j="eif.a1.ga0", value = data[["eif.a1.ga0"]] + 
          data[[paste0("H.a1.ga0.", i)]]*(data[[paste0("QM_", i, ".a1.ga0")]] - data[[paste0("QL_", i, ".a1.ga0.star")]]))
    set(data, j="eif.a0.ga0", value = data[["eif.a0.ga0"]] + 
          data[[paste0("H.a0.ga0.", i)]]*(data[[paste0("QM_", i, ".a0.ga0")]] - data[[paste0("QL_", i, ".a0.ga0.star")]]))
  }
  
  ### Estimands ###
  sde <- data[data[[Cnodes[1]]]==0, mean(QL_1.a1.ga0.star)-mean(QL_1.a0.ga0.star)] 
  sie <- data[data[[Cnodes[1]]]==0, mean(QL_1.a1.ga1.star)-mean(QL_1.a1.ga0.star)] 
  oe <- data[data[[Cnodes[1]]]==0, mean(QL_1.a1.ga1.star)-mean(QL_1.a0.ga0.star)] 
  sdevar <- data[, var(na.omit(eif.a1.ga0 - eif.a0.ga0)) / n]
  sievar <- data[, var(na.omit(eif.a1.ga1 - eif.a1.ga0)) / n]
  oevar <- data[, var(na.omit(eif.a1.ga1 - eif.a0.ga0)) / n]
  sdese <- data[, sqrt(mean(na.omit(eif.a1.ga0 - eif.a0.ga0)^2) / n)]
  siese <- data[, sqrt(mean(na.omit(eif.a1.ga1 - eif.a1.ga0)^2) / n)]
  oese <- data[, sqrt(mean(na.omit(eif.a1.ga1 - eif.a0.ga0)^2) / n)]
  out <- data.table("sde"=sde, "sdevar"=sdevar, "sdese"=sdese, 
                    "sie"=sie, "sievar"=sievar, "siese"=siese,
                    "oe"=oe, "oevar"=oevar, "oese"=oese)
  class(out) <- "fitLTMLE"
  return(out)
}  
