#This is the total number of jobs that you told Slurm to execute
number_of_tasks <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
#This is an index specific for each job running in parallel
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Set a reproducible random seed for your simulations
set.seed(123457890)
# Generate a large number of random seeds to be assigned to each task
seeds <- sample(1:10^6, size = number_of_tasks, replace = FALSE)
# Assign the randomly generated seed to the current task
set.seed(seeds[task_id])

# ---------- load packages ----------
library(data.table)
library(sl3)

#----------- setting --------------
n <- 400 # sample size
setting <- 1 # data-generating mechanism
formula <- "correct" # g-formulas
B <- 500 # number of bootstrap reps

#----------- source fcts ------------
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("Functions",".R$",full.names=TRUE)){source(f)}
source("./settings/simulation_settings.R")

#----------- R code ----------------
data <- do.call(simulateData, c(list(n=n), setting))
fit <- do.call(fitLTMLE, c(nodes, formula, list(data=data, a1=1, a0=0)))
trueVal <- do.call(theTruth, c(setting, list(n=10^6, fitg = fit$fitg)))
result <- cbind(fit$est.all, trueVal)

if(B>0){
  boot.t <- matrix(NA, nrow= B, ncol = length(fit$est.all))
  for(i in 1:B){
    indices <- sample(c(1:n), size=n, replace=TRUE)
    bootdata <- data[indices, ]
    fit.boot <- do.call(fitLTMLE, c(nodes, formula, list(data=bootdata, a1=1, a0=0)))
    boot.t[i, ] <- fit.boot$est.all
  }
  
  sd.boot <- apply(boot.t, 1, sd)
  result <- cbind(result, sd.boot)
}
# -------------------------------------------------------------------
# Save the results for this task as an individual file in the output folder
save(result, file = paste0('table1-n', paste0(n), '-setting', paste0(setting), 
                           '-formula_', paste0(formula), '-',  sprintf("%04d", task_id), '.RData'))