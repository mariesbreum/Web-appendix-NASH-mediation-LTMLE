# ---------- load packages ----------
library(data.table)
library(sl3)

#----------- source fcts -----------
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}

#----------- R code ----------------
oneRun <- function(n, setting, formula){
  data <- do.call(simulateData, c(list(n=n), setting));
  fit <- do.call(fitLTMLE, c(list(data=data, a1=1, a0=0), nodes, formula));
  trueVal <- do.call(theTruth, c(setting, list(n=10^6, fitg = fit$fitg, pi=0.5)))
  res <- cbind(fit$est.all, trueVal)
  res
}

#---------- simulations -----------
number_of_tasks <- 1000
set.seed(123457890)
seeds <- sample(1:10^6, size = number_of_tasks, replace = FALSE)
res <- matrix(NA, number_of_tasks, 42)
for(task_id in 1:number_of_tasks){
  set.seed(seeds[task_id])
  res[task_id,] <- as.numeric(oneRun(n, setting, formula))
}
colnames(res) <- c("est.ide", "est.iie", "est.oe", "est.pm", "est.gide",    
                   "se.ide", "se.iie", "se.oe", "se.pm", "se.gide",     
                   "est.psi11", "est.psi10", "est.psi00", "est.psi1g", "est.psi0g",   
                   "se.psi11", "se.psi10", "se.psi00", "se.psi1g", "se.psi0g",    
                   "est.ORide", "est.ORiie", "est.ORoe", "est.ORgide","se.ORide",    
                   "se.ORiie", "se.ORoe", "se.ORgide", "psi11.true","psi10.true",  
                   "psi00.true","psi1g.true", "psi0g.true","ide.true","iie.true",    
                   "oe.true", "pm.true", "gide.true","ORide.true", "ORiie.true", 
                   "ORoe.true", "OR.gide.true")
saveRDS(res, file = paste0('res-n', n, '-', name, '.RDS'))