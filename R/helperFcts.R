# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------
# R code:
#
make_task <- function(data, model){
  
  model <- as.formula(model)
  
  if(length(all.vars(model))<=1){
    data$int <- rep(1, nrow(data))
    sl3::sl3_Task$new(
      data = data,
      covariates = "int",
      outcome = all.vars(model)[1]
    )
  }
  else{
    sl3::sl3_Task$new(
      data = data,
      covariates = all.vars(model)[2:length(all.vars(model))],
      outcome = all.vars(model)[1]
    )
  }
} 


make_prediction_task <- function(preddata, model){
  
  model <- as.formula(model)
  
  if(length(all.vars(model))<=1){
    data$int <- rep(1, nrow(data))
    sl3::sl3_Task$new(
      data = preddata,
      covariates = "int",
    )
  }
  else{
    sl3::sl3_Task$new(
      data = preddata,
      covariates = all.vars(model)[2:length(all.vars(model))],
    )
  }
} 

