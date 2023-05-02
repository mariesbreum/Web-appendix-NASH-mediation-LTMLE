# -----------------------------------------------------------------------------
# Functions - LTMLE 
# -----------------------------------------------------------------------------
# R code:
#

fitLTMLE <- function(data, # data table or data frame 
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
                     RYmodel, # model for R.Y
                     Ymodel, # model for initial regression QY
                     QLcov, # list of covariates for regressions QL_i
                     a = 1, # level of treatment var corresponding to a
                     a.prime = 0, # level of treatment var corresponding to a'
                     n_bins = 20,
                     Ylearner = NULL,
                     RYlearner = NULL,
                     Mlearner = NULL,
                     QLlearner = NULL,
                     Clearner = NULL,
                     lwr = 1e-09,
                     upr = 1-1e-09
                     
){
 data.table::setDT(data)
 cols <- c(L0nodes, Anode, Cnodes, Lnodes, Mnodes, RYnode, Ynode)
 data <- data.table::copy(data)[,..cols]
 
 # set-up 
 K <- length(t)
 n <- nrow(data)
 
 # Learners
 if(is.null(Ylearner)){
   lrn_Y <- sl3::Lrnr_glm$new()
 }
 else{
   lrn_Y <- Ylearner
 }
 if(is.null(QLlearner)){
   stack <- sl3::Stack$new(Lrnr_glm$new(), Lrnr_mean$new(), Lrnr_bayesglm$new(), 
                           Lrnr_gam$new(), Lrnr_caret$new(algorithm  = "glmStepAIC", trace=F))
   corP_screen <- sl3::Lrnr_screener_correlation$new(type = "threshold", 
                                                pvalue_threshold = 0.05, min_screen = 1)
   lrn_QL <- sl3::Lrnr_sl$new(learners = Stack$new(stack, Pipeline$new(corP_screen, stack)))
 }
 else{
   lrn_QL <- QLlearner
 }
 if(is.null(Mlearner)){
   lrn_g <- sl3::Lrnr_density_discretize$new(categorical_learner=Lrnr_glmnet$new(n_bins=n_bins))
 }
 else{
   lrn_g <- Mlearner
 }
 
 # fit g model
 fitg <- list()
 for(i in 1:K){
   fitg[[i]] <- lrn_g$train(make_task(data[data[[Cnodes[i]]]==0,], Mmodel[[i]]))
 }
 
 # compute H weights
 data <- cbind(data, fitInitial(data, t, Anode, Cnodes, RYnode, Cmodel, Mmodel, RYmodel, a, a.prime,
                                Clearner, RYlearner, fitg))
 
 # fit initial Y model
 fitY <- lrn_Y$train(make_task(data[data[[RYnode]]==1,], Ymodel))
 
 ### Compute QY_star ###
 
 # initial predictions QY
 nd.a <- copy(data)[, A:=rep(a, n)]
 nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
 QY.a <- fitY$predict(make_prediction_task(nd.a[nd.a[[RYnode]]==1,], Ymodel))
 QY.aprime <- fitY$predict(make_prediction_task(nd.aprime[nd.aprime[[RYnode]]==1,], Ymodel))
 QY.a <- pmin(upr, pmax(lwr, QY.a))
 QY.aprime <- pmin(upr, pmax(lwr, QY.aprime))
 
 # target QY
 eps.a.ga <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                          weights = data[data[[RYnode]]==1,][[paste0("H.a.ga.", K+1)]], offset = qlogis(QY.a), 
                          family = quasibinomial()))
 eps.a.gaprime <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                               weights = data[data[[RYnode]]==1,][[paste0("H.a.gaprime.", K+1)]], offset = qlogis(QY.a), 
                               family = quasibinomial()))
 eps.aprime.gaprime <- coef(glm.fit(x=rep(1, nrow(data[data[[RYnode]]==1,])), y = data[data[[RYnode]]==1,][[Ynode]], 
                                    weights = data[data[[RYnode]]==1,][[paste0("H.aprime.gaprime.", K+1)]], offset = qlogis(QY.aprime), 
                                    family = quasibinomial()))
 
 # Update QY
 data[data[[RYnode]]==1, "QY_star.a.ga" := plogis(qlogis(QY.a) + eps.a.ga)]
 data[data[[RYnode]]==1, "QY_star.a.gaprime" := plogis(qlogis(QY.a) + eps.a.gaprime)]
 data[data[[RYnode]]==1, "QY_star.aprime.gaprime" := plogis(qlogis(QY.aprime) + eps.aprime.gaprime)]
 
 ### Compute QM_K ###
 
 # discrete grid m_K
 m_K_discrete <- make_bins(data[data[[Cnodes[K]]]==0, ][[Mnodes[K]]], type = "equal_range", n_bins = n_bins)
 
 # integrate out m_K
 data[, paste0("QM_", K, ".a.ga") := 0]
 data[, paste0("QM_", K, ".a.gaprime") := 0]
 data[, paste0("QM_", K, ".aprime.gaprime") := 0]
 for(i in 1:n_bins){
   nd.a.mk <- copy(nd.a)[,Mnodes[K]:=m_K_discrete[i]]
   nd.aprime.mk <- copy(nd.aprime)[,Mnodes[K]:=m_K_discrete[i]]
 
   # predict QY
   QY.a <- fitY$predict(make_task(nd.a.mk[nd.a.mk[[Cnodes[K]]]==0,], Ymodel))
   QY.aprime <- fitY$predict(make_task(nd.aprime.mk[nd.aprime.mk[[Cnodes[K]]]==0,], Ymodel))
   QY.a <- pmin(upr, pmax(lwr, QY.a))
   QY.aprime <- pmin(upr, pmax(lwr, QY.aprime))
   
   # predict g_a_K
   g.a <- fitg[[K]]$predict(make_task(nd.a.mk[nd.a.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))
   g.aprime <- fitg[[K]]$predict(make_task(nd.aprime.mk[nd.aprime.mk[[Cnodes[K]]]==0,], Mmodel[[K]]))

   set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a.ga"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a.ga")]] + plogis(qlogis(QY.a) + eps.a.ga)*g.a*diff(m_K_discrete)[i])
   set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a.gaprime"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a.gaprime")]] + plogis(qlogis(QY.a) + eps.a.gaprime)*g.aprime*diff(m_K_discrete)[i])
   set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".aprime.gaprime"), value = data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".aprime.gaprime")]] + plogis(qlogis(QY.aprime) + eps.aprime.gaprime)*g.aprime*diff(m_K_discrete)[i])
 }
 
 set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a.ga"), value =pmin(1, pmax(0,data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a.ga")]])))
 set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".a.gaprime"), value =pmin(1, pmax(0,data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".a.gaprime")]])))
 set(data, i = which(data[[Cnodes[K]]]==0), j = paste0("QM_", K, ".aprime.gaprime"), value =pmin(1, pmax(0,data[data[[Cnodes[K]]]==0, ][[paste0("QM_", K, ".aprime.gaprime")]])))
 

 for(k in 1:(K-1)){
   
   ### Compute QL_k+1_star ###
   
   # fit initial QL_k+1
   fitL.a.ga <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                        paste0("QM_",K-k+1, ".a.ga~",QLcov[K-k+1])))
   fitL.a.gaprime <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                             paste0("QM_",K-k+1, ".a.gaprime~",QLcov[K-k+1])))
   fitL.aprime.gaprime <- lrn_QL$train(make_task(data[data[[Cnodes[K-k+1]]]==0,], 
                                                  paste0("QM_",K-k+1, ".aprime.gaprime~",QLcov[K-k+1])))
   
   # initial predictions QL_k+1
   nd.a <- copy(data)[, A:=rep(a, n)]
   nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
   QL.a.ga <- fitL.a.ga$predict(make_task(nd.a[nd.a[[Cnodes[K-k+1]]]==0,], 
                                          paste0("QM_",K-k+1, ".a.ga", "~",QLcov[K-k+1])))
   QL.a.gaprime <- fitL.a.gaprime$predict(make_task(nd.a[nd.a[[Cnodes[K-k+1]]]==0,], 
                                                    paste0("QM_",K-k+1, ".a.gaprime", "~",QLcov[K-k+1])))
   QL.aprime.gaprime <- fitL.aprime.gaprime$predict(make_task(nd.aprime[nd.aprime[[Cnodes[K-k+1]]]==0,],
                                                              paste0("QM_",K-k+1, ".aprime.gaprime", "~",QLcov[K-k+1])))
   QL.a.ga <- pmin(upr, pmax(lwr, QL.a.ga))
   QL.a.gaprime <- pmin(upr, pmax(lwr, QL.a.gaprime))
   QL.aprime.gaprime <- pmin(upr, pmax(lwr, QL.aprime.gaprime))
   
   # target QL_k+1
   eps.a.ga <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".a.ga")]], 
                            weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.a.ga.", K-k+1)]], offset = qlogis(QL.a.ga), 
                            family = quasibinomial()))
   
   eps.a.gaprime <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".a.gaprime")]], 
                                 weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.a.gaprime.", K-k+1)]], offset = qlogis(QL.a.gaprime), 
                                 family = quasibinomial()))
   
   eps.aprime.gaprime<- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[K-k+1]]]==0,])), y = data[data[[Cnodes[K-k+1]]]==0,][[paste0("QM_",K-k+1, ".aprime.gaprime")]], 
                                     weights = data[data[[Cnodes[K-k+1]]]==0,][[paste0("H.aprime.gaprime.",K-k+1)]], offset = qlogis(QL.aprime.gaprime), 
                                     family = quasibinomial()))
   
   data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".a.ga.star"):= plogis(qlogis(QL.a.ga) + eps.a.ga)]
   data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".a.gaprime.star"):= plogis(qlogis(QL.a.gaprime) + eps.a.gaprime)]
   data[data[[Cnodes[K-k+1]]]==0, paste0("QL_", K-k+1, ".aprime.gaprime.star"):= plogis(qlogis(QL.aprime.gaprime) + eps.aprime.gaprime)]
   
   ### Compute QM_k ###
  
   # discrete grid m_k
   m_k_discrete <- make_bins(data[data[[Cnodes[K-k]]]==0, ][[Mnodes[K-k]]], type="equal_range", n_bins=n_bins)
   
   # integrate out m_k
   data[, paste0("QM_", K-k, ".a.ga") :=0]
   data[, paste0("QM_", K-k, ".a.gaprime") :=0]
   data[, paste0("QM_", K-k, ".aprime.gaprime") :=0]
   
   for(i in 1:n_bins){
     nd.a.mk <- copy(nd.a)[,Mnodes[K-k]:=m_k_discrete[i]]
     nd.aprime.mk <- copy(nd.aprime)[,Mnodes[K-k]:=m_k_discrete[i]]
     
     # predictions QL_k+1
     QL.a.ga <- fitL.a.ga$predict(make_task(nd.a.mk[nd.a.mk[[Cnodes[K-k]]]==0,], 
                                            paste0("QM_",K-k+1, ".a.ga", "~",QLcov[K-k+1])))
     QL.a.gaprime <- fitL.a.gaprime$predict(make_task(nd.a.mk[nd.a.mk[[Cnodes[K-k]]]==0,], 
                                                      paste0("QM_",K-k+1, ".a.gaprime", "~",QLcov[K-k+1])))
     QL.aprime.gaprime <- fitL.aprime.gaprime$predict(make_task(nd.aprime.mk[nd.aprime.mk[[Cnodes[K-k]]]==0,],
                                                                paste0("QM_",K-k+1, ".aprime.gaprime", "~",QLcov[K-k+1])))
     QL.a.ga <- pmin(upr, pmax(lwr, QL.a.ga))
     QL.a.gaprime <- pmin(upr, pmax(lwr, QL.a.gaprime))
     QL.aprime.gaprime <- pmin(upr, pmax(lwr, QL.aprime.gaprime))
     
     # predict g_a_k
     g.a <-fitg[[K-k]]$predict(make_task(nd.a.mk[nd.a.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))
     g.aprime <- fitg[[K-k]]$predict(make_task(nd.aprime.mk[nd.aprime.mk[[Cnodes[K-k]]]==0,], Mmodel[[K-k]]))
     
     set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a.ga"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a.ga")]] + plogis(qlogis(QL.a.ga) + eps.a.ga)*g.a*diff(m_k_discrete)[i])
     set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a.gaprime"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a.gaprime")]] + plogis(qlogis(QL.a.gaprime) + eps.a.gaprime)*g.aprime*diff(m_k_discrete)[i])
     set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".aprime.gaprime"), value = data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".aprime.gaprime")]] + plogis(qlogis(QL.aprime.gaprime) + eps.aprime.gaprime)*g.aprime*diff(m_k_discrete)[i])
     
   }
   set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a.ga"), value = pmin(1, pmax(0,data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a.ga")]])))
   set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".a.gaprime"), value = pmin(1, pmax(0,data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".a.gaprime")]])))
   set(data, i = which(data[[Cnodes[K-k]]]==0), j = paste0("QM_", K-k, ".aprime.gaprime"), value = pmin(1, pmax(0,data[data[[Cnodes[K-k]]]==0, ][[paste0("QM_", K-k, ".aprime.gaprime")]])))
   
 }
 
 ### Compute QL_1_star ###
 
 # fit initial QL_1
 fitL.a.ga <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a.ga", "~",QLcov[1])))
 fitL.a.gaprime <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.a.gaprime", "~",QLcov[1])))
 fitL.aprime.gaprime <- lrn_QL$train(make_task(data[data[[Cnodes[1]]]==0,], paste0("QM_1.aprime.gaprime", "~",QLcov[1])))
 
 # initial predictions QL_1
 nd.a <- copy(data)[, A:=rep(a, n)]
 nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
 QL1.a.ga <- fitL.a.ga$predict(make_task(nd.a[nd.a[[Cnodes[1]]]==0,], 
                                         paste0("QM_1.a.ga", "~",QLcov[1])))
 QL1.a.gaprime <- fitL.a.gaprime$predict(make_task(nd.a[nd.a[[Cnodes[1]]]==0,], 
                                                   paste0("QM_1.a.gaprime", "~",QLcov[1])))
 QL1.aprime.gaprime <- fitL.aprime.gaprime$predict(make_task(nd.aprime[nd.aprime[[Cnodes[1]]]==0,], 
                                                             paste0("QM_1.aprime.gaprime", "~",QLcov[1])))
 
 QL1.a.ga <- pmin(upr, pmax(lwr, QL1.a.ga))
 QL1.a.gaprime <- pmin(upr, pmax(lwr, QL1.a.gaprime))
 QL1.aprime.gaprime <- pmin(upr, pmax(lwr, QL1.aprime.gaprime))
 
 # target QL_1
 eps.a.ga <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.a.ga"]], 
                          weights = data[data[[Cnodes[1]]]==0,][[paste0("H.a.ga.", 1)]], offset = qlogis(QL1.a.ga), 
                          family = quasibinomial()))
 eps.a.gaprime<- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.a.gaprime"]], 
                              weights = data[data[[Cnodes[1]]]==0,][[paste0("H.a.gaprime.", 1)]], offset = qlogis(QL1.a.gaprime), 
                              family = quasibinomial()))
 eps.aprime.gaprime <- coef(glm.fit(x = rep(1, nrow(data[data[[Cnodes[1]]]==0,])), y = data[data[[Cnodes[1]]]==0,][["QM_1.aprime.gaprime"]], 
                                    weights = data[data[[Cnodes[1]]]==0,][[paste0("H.aprime.gaprime.", 1)]], offset = qlogis(QL1.aprime.gaprime), 
                                    family = quasibinomial()))
 
 set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.a.ga.star", value=plogis(qlogis(QL1.a.ga) + eps.a.ga))
 set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.a.gaprime.star", value=plogis(qlogis(QL1.a.gaprime) + eps.a.gaprime))
 set(data, i=which(data[[Cnodes[1]]]==0), j="QL_1.aprime.gaprime.star", value=plogis(qlogis(QL1.aprime.gaprime) + eps.aprime.gaprime))
 
 ### Compute eif ###
 set(data, j="eif.a.ga", value=data[[paste0("H.a.ga.", K+1)]]*(data[[Ynode]]-data[["QY_star.a.ga"]]))
 set(data, j="eif.a.gaprime", value=data[[paste0("H.a.gaprime.", K+1)]]*(data[[Ynode]]-data[["QY_star.a.gaprime"]]))
 set(data, j="eif.aprime.gaprime", value=data[[paste0("H.aprime.gaprime.", K+1)]]*(data[[Ynode]]-data[["QY_star.aprime.gaprime"]]))
 
 for(i in 1:K){
   set(data, j="eif.a.ga", value = data[["eif.a.ga"]] + data[[paste0("H.a.ga.", i)]]*(data[[paste0("QM_", i, ".a.ga")]]-
                                                                                        data[[paste0("QL_", i, ".a.ga.star")]]))
   set(data, j="eif.a.gaprime", value = data[["eif.a.gaprime"]] + data[[paste0("H.a.gaprime.", i)]]*(data[[paste0("QM_", i, ".a.gaprime")]]-
                                                                    data[[paste0("QL_", i, ".a.gaprime.star")]]))
   set(data, j="eif.aprime.gaprime", value= data[["eif.aprime.gaprime"]] + data[[paste0("H.aprime.gaprime.", i)]]*(data[[paste0("QM_", i, ".aprime.gaprime")]]-
                                                                    data[[paste0("QL_", i, ".aprime.gaprime.star")]]))
 }
 
 ### Estimands ###
 sde <- data[data[[Cnodes[1]]]==0, mean(QL_1.a.gaprime.star)-mean(QL_1.aprime.gaprime.star)] 
 sie <- data[data[[Cnodes[1]]]==0, mean(QL_1.a.ga.star)-mean(QL_1.a.gaprime.star)] 
 sdevar <- data[, var(na.omit(eif.a.gaprime - eif.aprime.gaprime)) / n]
 sievar <- data[, var(na.omit(eif.a.ga - eif.a.gaprime)) / n]
 out <- list("sde"=sde, "sdevar"=sdevar, "sie"=sie, "sievar"=sievar)
 class(out) <- "fitLTMLE"
 return(out)
}  

