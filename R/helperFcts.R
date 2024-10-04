# ------------------------------------------------------------------------------
# helperFcts.R
# ------------------------------------------------------------------------------
#
make_task <- function(data, model){
  
  model <- as.formula(model)
  
  if(length(all.vars(model))>1){
    sl3::sl3_Task$new(
      data = data,
      covariates = all.vars(model)[2:length(all.vars(model))],
      outcome = all.vars(model)[1]
    )
  }
  else{
    sl3::sl3_Task$new(
      data = data,
      covariates = NULL,
      outcome = all.vars(model)[1]
    )
  }
} 

make_prediction_task <- function(preddata, model){
  
  model <- as.formula(model)
  
  if(length(all.vars(model))>1){
    sl3::sl3_Task$new(
      data = preddata,
      covariates = all.vars(model)[2:length(all.vars(model))]    
      )
  }
  else{
    sl3::sl3_Task$new(
      data = preddata,
      covariates = NULL
    )
  }
} 
#-------------------------------------------------------------------------------