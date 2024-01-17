# -----------------------------------------------------------------------------
# summary.fitLTMLE
# -----------------------------------------------------------------------------
# Description:
# summary function for the fitLTMLE fct
# 
summary.fitLTMLE <- function(obj, type="diff", conf.int=0.95){
  if(type=="diff"){
    x <- obj$est.diff
    out <- data.frame("est" = c(x$est.sde, x$est.sie, x$est.oe, x$est.pm),
                      "se" = c(x$se.sde, x$se.sie, x$se.oe, x$se.pm),
                      "CI.low" = c(x$est.sde-qnorm((1+conf.int)/2)*x$se.sde, 
                                   x$est.sie-qnorm((1+conf.int)/2)*x$se.sie, 
                                   x$est.oe-qnorm((1+conf.int)/2)*x$se.oe,
                                   x$est.pm-qnorm((1+conf.int)/2)*x$se.pm),
                      "CI.up" = c(x$est.sde+qnorm((1+conf.int)/2)*x$se.sde, 
                                  x$est.sie+qnorm((1+conf.int)/2)*x$se.sie, 
                                  x$est.oe+qnorm((1+conf.int)/2)*x$se.oe,
                                  x$est.pm+qnorm((1+conf.int)/2)*x$se.pm))
    rownames(out) <- c("sde", "sie", "oe", "pm")
  }
  if(type=="OR"){
    x <- obj$est.OR
    out <- data.frame("est" = c(x$est.ORsde, x$est.ORsie, x$est.ORoe),
                      "se" = c(x$se.ORsde, x$se.ORsie, x$se.ORoe),
                      "CI.low" = c(x$est.ORsde-qnorm((1+conf.int)/2)*x$se.ORsde, 
                                   x$est.ORsie-qnorm((1+conf.int)/2)*x$se.ORsie, 
                                   x$est.ORoe-qnorm((1+conf.int)/2)*x$se.ORoe),
                      "CI.up" = c(x$est.ORsde+qnorm((1+conf.int)/2)*x$se.ORsde, 
                                  x$est.ORsie+qnorm((1+conf.int)/2)*x$se.ORsie, 
                                  x$est.ORoe+qnorm((1+conf.int)/2)*x$se.ORoe))
    rownames(out) <- c("OR_sde", "OR_sie", "OR_oe")
  }
  if(type=="logOR"){
    x <- obj$est.logOR
    out <- data.frame("est" = c(x$est.logORsde, x$est.logORsie, x$est.logORoe, x$est.logORpm),
                      "se" = c(x$se.ORsde, x$se.ORsie, x$se.ORoe, x$se.logORpm),
                      "CI.low" = c(x$est.logORsde-qnorm((1+conf.int)/2)*x$se.logORsde, 
                                   x$est.logORsie-qnorm((1+conf.int)/2)*x$se.logORsie, 
                                   x$est.logORoe-qnorm((1+conf.int)/2)*x$se.logORoe,
                                   x$est.logORpm-qnorm((1+conf.int)/2)*x$se.logORpm),
                      "CI.up" = c(x$est.logORsde+qnorm((1+conf.int)/2)*x$se.logORsde, 
                                  x$est.logORsie+qnorm((1+conf.int)/2)*x$se.logORsie, 
                                  x$est.logORoe+qnorm((1+conf.int)/2)*x$se.logORoe,
                                  x$est.logORpm+qnorm((1+conf.int)/2)*x$se.logORpm))
    rownames(out) <- c("logOR_sde", "logOR_sie", "logOR_oe", "logOR_pm")
  }
  if(type=="psi"){
    x <- obj$est.psi
    out <- data.frame("est" = c(x$est.psi11, x$est.psi10, x$est.psi00),
                      "se" = c(x$se.psi11, x$se.psi10, x$se.psi00),
                      "CI.low" = c(x$est.psi11-qnorm((1+conf.int)/2)*x$se.psi11, 
                                   x$est.psi10-qnorm((1+conf.int)/2)*x$se.psi10, 
                                   x$est.psi00-qnorm((1+conf.int)/2)*x$se.psi00),
                      "CI.up" = c(x$est.psi11+qnorm((1+conf.int)/2)*x$se.psi11, 
                                  x$est.psi10+qnorm((1+conf.int)/2)*x$se.psi10, 
                                  x$est.psi00+qnorm((1+conf.int)/2)*x$se.psi00))
    rownames(out) <- c("psi_11", "psi_10", "psi_00")
  }
  return(out)
}


