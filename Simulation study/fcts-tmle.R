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
                     n_bins=20,
                     Ylearner=NULL,
                     RYlearner=NULL,
                     Mlearner=NULL,
                     QLlearner=NULL,
                     Clearner=NULL
){
 data.table::setDT(data)
 cols <- c(L0nodes, Anode, Cnodes, Lnodes, Mnodes, RYnode, Ynode)
 data <- data.table::copy(data)[,..cols]
 
 # set-up 
 K <- length(t)
 n <- nrow(data)
 
 # bounds for prob estimates
 lwr <- 0.0001
 upr <- 0.9999
 
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
   fitg[[i]] <- lrn_g$train(make_task(data[get(Cnodes[i])==0,], Mmodel[[i]]))
 }
 
 # compute H weights
 data <- cbind(data, fitInitial(data, t, Anode, Cnodes, RYnode, Cmodel, Mmodel, RYmodel, a, a.prime,
                                Clearner, RYlearner, fitg))
 
 # fit initial Y model
 fitY <- lrn_Y$train(make_task(data[get(RYnode)==1,], Ymodel))
 
 ### Compute QY_star ###
 
 # initial predictions QY
 nd.a <- copy(data)[, A:=rep(a, n)]
 nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
 QY.a <- fitY$predict(make_prediction_task(nd.a[get(RYnode)==1,], Ymodel))
 QY.aprime <- fitY$predict(make_prediction_task(nd.aprime[get(RYnode)==1,], Ymodel))

 # target QY
 eps.a.ga <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.a.ga.", K+1)) , 
                      offset = qlogis(QY.a), family = "quasibinomial", data = data[get(RYnode)==1,]))
 eps.a.gaprime <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.a.gaprime.", K+1)), 
                           offset = qlogis(QY.a), family = "quasibinomial", data = data[get(RYnode)==1,]))
 eps.aprime.gaprime <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.aprime.gaprime.", K+1)), 
                            offset = qlogis(QY.aprime), family = "quasibinomial", data = data[get(RYnode)==1,]))
 
 
 # Update QY
 data[get(RYnode)==1, paste0("QY_star.a.ga")  := plogis(qlogis(QY.a) + eps.a.ga)]
 data[get(RYnode)==1, paste0("QY_star.a.gaprime") := plogis(qlogis(QY.a) + eps.a.gaprime)]
 data[get(RYnode)==1, paste0("QY_star.aprime.gaprime") :=  plogis(qlogis(QY.aprime) + eps.aprime.gaprime)]
 
 ### Compute QM_K ###
 
 # discrete grid m_K
 m_K_discrete <- make_bins(data[get(Cnodes[K])==0, get(Mnodes[K])], type = "equal_range", n_bins = n_bins)
 
 # integrate out m_K
 data[, paste0("QM_", K, ".a.ga") := 0]
 data[, paste0("QM_", K, ".a.gaprime") := 0]
 data[, paste0("QM_", K, ".aprime.gaprime") := 0]
 
 for(i in 1:n_bins){
   nd.a.mK <- copy(nd.a)[,Mnodes[K]:=m_K_discrete[i]]
   nd.aprime.mK <- copy(nd.aprime)[,Mnodes[K]:=m_K_discrete[i]]
 
   # initial predictions QY
   QY.a <- fitY$predict(make_task(nd.a.mK[get(Cnodes[K])==0,], Ymodel))
   QY.aprime <- fitY$predict(make_task(nd.aprime.mK[get(Cnodes[K])==0,], Ymodel))
    
   # target QY
   eps.a.ga <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.a.ga.", K+1)) , 
                        offset = qlogis(QY.a), family = "quasibinomial", data = data[get(Cnodes[K])==0,]))
   eps.a.gaprime <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.a.gaprime.", K+1)), 
                             offset = qlogis(QY.a), family = "quasibinomial", data = data[get(Cnodes[K])==0,]))
   eps.aprime.gaprime <- coef(glm(as.formula(paste(Ynode, 1, sep="~")), weights = get(paste0("H.aprime.gaprime.", K+1)), 
                              offset = qlogis(QY.aprime), family = "quasibinomial", data = data[get(Cnodes[K])==0,]))
   
   # predict g_a_K
   g.a <-fitg[[K]]$predict(make_task(nd.a.mK[get(Cnodes[K])==0,], Mmodel[[K]]))
   g.aprime <- fitg[[K]]$predict(make_task(nd.aprime.mK[get(Cnodes[K])==0,], Mmodel[[K]]))

   data[get(Cnodes[K])==0, paste0("QM_", K, ".a.ga")  := get(paste0("QM_", K, ".a.ga")) + plogis(qlogis(QY.a) + eps.a.ga)*g.a*diff(m_K_discrete)[i]]
   data[get(Cnodes[K])==0, paste0("QM_", K, ".a.gaprime") := get(paste0("QM_", K, ".a.gaprime")) + plogis(qlogis(QY.a) + eps.a.gaprime)*g.aprime*diff(m_K_discrete)[i]]
   data[get(Cnodes[K])==0, paste0("QM_", K, ".aprime.gaprime") := get(paste0("QM_", K, ".aprime.gaprime")) + plogis(qlogis(QY.aprime) + eps.aprime.gaprime)*g.aprime*diff(m_K_discrete)[i]]
 }
 data[get(paste0("QM_", K, ".a.ga"))< 0.0, paste0("QM_", K, ".a.ga"):=lwr]
 data[get(paste0("QM_", K, ".a.ga"))> 1, paste0("QM_", K, ".a.ga"):=upr]
 data[get(paste0("QM_", K, ".a.gaprime"))< 0.0, paste0("QM_", K, ".a.gaprime"):=lwr]
 data[get(paste0("QM_", K, ".a.gaprime"))> 1.0, paste0("QM_", K, ".a.gaprime"):=upr]
 data[get(paste0("QM_", K, ".aprime.gaprime"))< 0.0, paste0("QM_", K, ".aprime.gaprime"):=lwr]
 data[get(paste0("QM_", K, ".aprime.gaprime"))> 1.0, paste0("QM_", K, ".aprime.gaprime"):=upr]
 

 for(k in 1:(K-1)){
   
   ### Compute QL_k+1_star ###
   
   # fit initial QL_k+1
   fitL.a.ga <- lrn_QL$train(make_task(data[get(Cnodes[K-k+1])==0,], 
                                        paste0("QM_",K-k+1, ".a.ga~",QLcov[K-k+1])))
   fitL.a.gaprime <- lrn_QL$train(make_task(data[get(Cnodes[K-k+1])==0,], 
                                             paste0("QM_",K-k+1, ".a.gaprime~",QLcov[K-k+1])))
   fitL.aprime.gaprime <- lrn_QL$train(make_task(data[get(Cnodes[K-k+1])==0,], 
                                                  paste0("QM_",K-k+1, ".aprime.gaprime~",QLcov[K-k+1])))
   
   # initial predictions QL_k+1
   nd.a <- copy(data)[, A:=rep(a, n)]
   nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
   QL.a.ga <- fitL.a.ga$predict(make_task(nd.a[get(Cnodes[K-k+1])==0,], 
                                          paste0("QM_",K-k+1, ".a.ga", "~",QLcov[K-k+1])))
   QL.a.gaprime <- fitL.a.gaprime$predict(make_task(nd.a[get(Cnodes[K-k+1])==0,], 
                                                    paste0("QM_",K-k+1, ".a.gaprime", "~",QLcov[K-k+1])))
   QL.aprime.gaprime <- fitL.aprime.gaprime$predict(make_task(nd.aprime[get(Cnodes[K-k+1])==0,],
                                                              paste0("QM_",K-k+1, ".aprime.gaprime", "~",QLcov[K-k+1])))
   QL.a.ga[QL.a.ga>1]<-upr
   QL.a.ga[QL.a.ga<0]<-lwr
   QL.a.gaprime[QL.a.gaprime>1]<-upr
   QL.a.gaprime[QL.a.gaprime<0]<-lwr
   QL.aprime.gaprime[QL.aprime.gaprime>1]<-upr
   QL.aprime.gaprime[QL.aprime.gaprime<0]<-lwr
   
   # target QL_k+1
   eps.a.ga <- coef(glm(as.formula(paste0("QM_",K-k+1, ".a.ga~1")), weights = get(paste0("H.a.ga.", K-k+1)), 
                        offset = qlogis(QL.a.ga), family = "quasibinomial", data = data[get(Cnodes[K-k+1])==0,]))
   eps.a.gaprime<- coef(glm(as.formula(paste0("QM_",K-k+1, ".a.gaprime~1")), weights = get(paste0("H.a.gaprime.", K-k+1)), 
                            offset = qlogis(QL.a.gaprime), family = "quasibinomial" , data = data[get(Cnodes[K-k+1])==0,]))
   eps.aprime.gaprime<- coef(glm(as.formula(paste0("QM_",K-k+1, ".aprime.gaprime~1")), weights = get(paste0("H.aprime.gaprime.",K-k+1)), 
                                 offset = qlogis(QL.aprime.gaprime), family = "quasibinomial" , data = data[get(Cnodes[K-k+1])==0,]))
   
   data[get(Cnodes[K-k+1])==0, paste0("QL_", K-k+1, ".a.ga.star"):= plogis(qlogis(QL.a.ga) + eps.a.ga)]
   data[get(Cnodes[K-k+1])==0, paste0("QL_", K-k+1, ".a.gaprime.star"):= plogis(qlogis(QL.a.gaprime) + eps.a.gaprime)]
   data[get(Cnodes[K-k+1])==0, paste0("QL_", K-k+1, ".aprime.gaprime.star"):= plogis(qlogis(QL.aprime.gaprime) + eps.aprime.gaprime)]
   
   ### Compute QM_k ###
  
   # discrete grid m_k
   m_k_discrete <- make_bins(data[get(Cnodes[K-k])==0, get(Mnodes[K-k])], type="equal_range", n_bins=n_bins)
   
   # integrate out m_k
   data[, paste0("QM_", K-k, ".a.ga") :=0]
   data[, paste0("QM_", K-k, ".a.gaprime") :=0]
   data[, paste0("QM_", K-k, ".aprime.gaprime") :=0]
   
   for(i in 1:n_bins){
     nd.a.mk <- copy(nd.a)[,Mnodes[K-k]:=m_k_discrete[i]]
     nd.aprime.mk <- copy(nd.aprime)[,Mnodes[K-k]:=m_k_discrete[i]]
     
     # initial predictions QL_k+1
     QL.a.ga <- fitL.a.ga$predict(make_task(nd.a.mk[get(Cnodes[K-k])==0,], 
                                            paste0("QM_",K-k+1, ".a.ga", "~",QLcov[K-k+1])))
     QL.a.gaprime <- fitL.a.gaprime$predict(make_task(nd.a.mk[get(Cnodes[K-k])==0,], 
                                                      paste0("QM_",K-k+1, ".a.gaprime", "~",QLcov[K-k+1])))
     QL.aprime.gaprime <- fitL.aprime.gaprime$predict(make_task(nd.aprime.mk[get(Cnodes[K-k])==0,],
                                                                paste0("QM_",K-k+1, ".aprime.gaprime", "~",QLcov[K-k+1])))
     QL.a.ga[QL.a.ga>1]<-upr
     QL.a.ga[QL.a.ga<0]<-lwr
     QL.a.gaprime[QL.a.gaprime>1]<-upr
     QL.a.gaprime[QL.a.gaprime<0]<-lwr
     QL.aprime.gaprime[QL.aprime.gaprime>1]<-upr
     QL.aprime.gaprime[QL.aprime.gaprime<0]<-lwr
     
     # target QL_k+1
     eps.a.ga <- coef(glm(as.formula(paste0("QM_",K-k+1, ".a.ga~1")), weights = get(paste0("H.a.ga.", K-k+1)), 
                          offset = qlogis(QL.a.ga), family = "quasibinomial", data = data[get(Cnodes[K-k])==0,]))
     eps.a.gaprime<- coef(glm(as.formula(paste0("QM_",K-k+1, ".a.gaprime~1")), weights = get(paste0("H.a.gaprime.", K-k+1)), 
                              offset = qlogis(QL.a.gaprime), family = "quasibinomial" , data = data[get(Cnodes[K-k])==0,]))
     eps.aprime.gaprime<- coef(glm(as.formula(paste0("QM_",K-k+1, ".aprime.gaprime~1")), weights = get(paste0("H.aprime.gaprime.",K-k+1)), 
                                   offset = qlogis(QL.aprime.gaprime), family = "quasibinomial" , data = data[get(Cnodes[K-k])==0,]))
     
     # predict g_a_k
     g.a <-fitg[[K-k]]$predict(make_task(nd.a.mk[get(Cnodes[K-k])==0,], Mmodel[[K-k]]))
     g.aprime <- fitg[[K-k]]$predict(make_task(nd.aprime.mk[get(Cnodes[K-k])==0,], Mmodel[[K-k]]))
     
     data[get(Cnodes[K-k])==0, paste0("QM_", K-k, ".a.ga") :=  get(paste0("QM_", K-k, ".a.ga")) + plogis(qlogis(QL.a.ga) + eps.a.ga)*g.a*diff(m_k_discrete)[i]]
     data[get(Cnodes[K-k])==0, paste0("QM_", K-k, ".a.gaprime") := get(paste0("QM_", K-k, ".a.gaprime")) + plogis(qlogis(QL.a.gaprime) + eps.a.gaprime)*g.aprime*diff(m_k_discrete)[i]]
     data[get(Cnodes[K-k])==0, paste0("QM_", K-k, ".aprime.gaprime") :=  get(paste0("QM_", K-k, ".aprime.gaprime")) + plogis(qlogis(QL.aprime.gaprime) + eps.aprime.gaprime)*g.aprime*diff(m_k_discrete)[i]]
   }
   data[get(paste0("QM_", K-k, ".a.ga")) <0.0, paste0("QM_", K-k, ".a.ga"):=lwr]
   data[get(paste0("QM_", K-k, ".a.ga")) >1.0, paste0("QM_", K-k, ".a.ga"):=upr]
   data[get(paste0("QM_", K-k, ".a.gaprime")) <0.0, paste0("QM_", K-k, ".a.gaprime"):=lwr]
   data[get(paste0("QM_", K-k, ".a.gaprime")) >1.0, paste0("QM_", K-k, ".a.gaprime"):=upr]
   data[get(paste0("QM_", K-k, ".aprime.gaprime")) <0.0, paste0("QM_", K-k, ".aprime.gaprime"):=lwr]
   data[get(paste0("QM_", K-k, ".aprime.gaprime")) >1.0, paste0("QM_", K-k, ".aprime.gaprime"):=upr]
   
 }
 
 ### Compute QL_1_star ###
 
 # fit initial QL_1
 fitL0.a.ga <- lrn_QL$train(make_task(data[get(Cnodes[1])==0,], paste0("QM_1.a.ga", "~",QLcov[1])))
 fitL0.a.gaprime <- lrn_QL$train(make_task(data[get(Cnodes[1])==0,], paste0("QM_1.a.gaprime", "~",QLcov[1])))
 fitL0.aprime.gaprime <- lrn_QL$train(make_task(data[get(Cnodes[1])==0,], paste0("QM_1.aprime.gaprime", "~",QLcov[1])))
 
 # initial predictions QL_1
 nd.a <- copy(data)[, A:=rep(a, n)]
 nd.aprime <- copy(data)[, A:=rep(a.prime, n)]
 QL1.a.ga <- fitL0.a.ga$predict(make_task(nd.a[get(Cnodes[1])==0,], 
                                         paste0("QM_1.a.ga", "~",QLcov[1])))
 QL1.a.gaprime <- fitL0.a.gaprime$predict(make_task(nd.a[get(Cnodes[1])==0,], 
                                                   paste0("QM_1.a.gaprime", "~",QLcov[1])))
 QL1.aprime.gaprime <- fitL0.aprime.gaprime$predict(make_task(nd.aprime[get(Cnodes[1])==0,], 
                                                             paste0("QM_1.aprime.gaprime", "~",QLcov[1])))
 
 QL1.a.ga[QL1.a.ga>1]<-upr
 QL1.a.ga[QL1.a.ga<0]<-lwr
 QL1.a.gaprime[QL1.a.gaprime>1]<-upr
 QL1.a.gaprime[QL1.a.gaprime<0]<-lwr
 QL1.aprime.gaprime[QL1.aprime.gaprime>1]<-upr
 QL1.aprime.gaprime[QL1.aprime.gaprime<0]<-lwr
 
 # target QL_1
 eps.a.ga <- coef(glm(QM_1.a.ga~1, weights = get(paste0("H.a.ga.", 1)), 
                      offset = qlogis(QL1.a.ga), family = "quasibinomial" , data = data[get(Cnodes[1])==0,]))
 eps.a.gaprime<- coef(glm(QM_1.a.gaprime~1, weights = get(paste0("H.a.gaprime.", 1)), 
                          offset = qlogis(QL1.a.gaprime), family = "quasibinomial", data = data[get(Cnodes[1])==0,]))
 eps.aprime.gaprime<- coef(glm(QM_1.aprime.gaprime~1, weights = get(paste0("H.aprime.gaprime.", 1)), 
                               offset = qlogis(QL1.aprime.gaprime), family = "quasibinomial" , data = data[get(Cnodes[1])==0,]))
 
 data[get(Cnodes[1])==0, QL_1.a.ga.star:=plogis(qlogis(QL1.a.ga) + eps.a.ga)]
 data[get(Cnodes[1])==0, QL_1.a.gaprime.star:=plogis(qlogis(QL1.a.gaprime) + eps.a.gaprime)]
 data[get(Cnodes[1])==0, QL_1.aprime.gaprime.star:=plogis(qlogis(QL1.aprime.gaprime) + eps.aprime.gaprime)]
 
 
 ### Compute eif ###
 data[, eif.a.ga := get(paste0("H.a.ga.", K+1))*(get(Ynode)-get("QY_star.a.ga"))]
 data[, eif.a.gaprime := get(paste0("H.a.gaprime.", K+1))*(get(Ynode)-get("QY_star.a.gaprime"))]
 data[, eif.aprime.gaprime := get(paste0("H.aprime.gaprime.", K+1))*(get(Ynode)-get("QY_star.aprime.gaprime"))]
 for(i in 1:K){
   data[, eif.a.ga := get("eif.a.ga") + get(paste0("H.a.ga.", i))*(get(paste0("QM_", i, ".a.ga"))-
                                                                    get(paste0("QL_", i, ".a.ga.star")))]
   data[, eif.a.gaprime := get("eif.a.gaprime") + get(paste0("H.a.gaprime.", i))*(get(paste0("QM_", i, ".a.gaprime"))-
                                                                    get(paste0("QL_", i, ".a.gaprime.star")))]
   data[, eif.aprime.gaprime := get("eif.aprime.gaprime") + get(paste0("H.aprime.gaprime.", i))*(get(paste0("QM_", i, ".aprime.gaprime"))-
                                                                    get(paste0("QL_", i, ".aprime.gaprime.star")))]
 }
 
 ### Estimands ###
 sde <- data[get(Cnodes[1])==0, mean(QL_1.a.gaprime.star)-mean(QL_1.aprime.gaprime.star)] 
 sie <- data[get(Cnodes[1])==0, mean(QL_1.a.ga.star)-mean(QL_1.a.gaprime.star)] 
 sdevar <- data[, var(na.omit(eif.a.gaprime - eif.aprime.gaprime)) / n]
 sievar <- data[, var(na.omit(eif.a.ga - eif.a.gaprime)) / n]
 
 return(list("sde"=sde, "sdevar"=sdevar, "sie"=sie, "sievar"=sievar))
}  
  