# -----------------------------------------------------------------------------
# summary.fitLTMLE
# -----------------------------------------------------------------------------
# Description:
# summary function for the fitLTMLE fct
# 
summary.fitLTMLE <- function(obj, type="diff", conf.int=0.95){
  if(type=="diff"){
    x <- obj$est.diff
    out <- data.frame("est" = c(x$est.sde, x$est.sie, x$est.oe),
                      "se" = c(x$se.sde, x$se.sie, x$se.oe),
                      "CI.low" = c(x$est.sde-qnorm((1+conf.int)/2)*x$se.sde, 
                                   x$est.sie-qnorm((1+conf.int)/2)*x$se.sie, 
                                   x$est.oe-qnorm((1+conf.int)/2)*x$se.oe),
                      "CI.up" = c(x$est.sde+qnorm((1+conf.int)/2)*x$se.sde, 
                                  x$est.sie+qnorm((1+conf.int)/2)*x$se.sie, 
                                  x$est.oe+qnorm((1+conf.int)/2)*x$se.oe))
    rownames(out) <- c("sde", "sie", "oe")
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
  if(type=="prop"){
    x <- obj$est.prop
    out <- data.frame("est" = c(x$est.propsde, x$est.propsie),
                      "se" = c(x$se.propsde, x$se.sie),
                      "CI.low" = c(x$est.propsde-qnorm((1+conf.int)/2)*x$se.propsde, 
                                   x$est.propsie-qnorm((1+conf.int)/2)*x$se.propsie),
                      "CI.up" = c(x$est.propsde+qnorm((1+conf.int)/2)*x$se.propsde, 
                                  x$est.propsie+qnorm((1+conf.int)/2)*x$se.propsie))
    rownames(out) <- c("prop_sde", "prop_sie")
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


