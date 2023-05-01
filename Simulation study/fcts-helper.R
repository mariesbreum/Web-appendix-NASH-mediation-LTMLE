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



make_bins <- function(x, type = c("equal_range", "equal_mass"), n_bins = NULL) {
  # clean up arguments
  type <- match.arg(type)
  
  # set grid along x
  if (type == "equal_range") {
    bins <- ggplot2::cut_interval(x, n_bins,
                                  right = FALSE,
                                  ordered_result = TRUE, dig.lab = 12
    )
  } else if (type == "equal_mass") {
    bins <- ggplot2::cut_number(x, n_bins,
                                right = FALSE,
                                ordered_result = TRUE, dig.lab = 12
    )
  }
  
  breaks_left <- as.numeric(sub(".(.+),.+", "\\1", levels(bins)))
  breaks_right <- as.numeric(sub(".+,(.+).", "\\1", levels(bins)))
  breaks <- c(breaks_left[1], breaks_right)
  return(breaks)
}

