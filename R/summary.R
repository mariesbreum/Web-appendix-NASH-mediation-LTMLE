# ------------------------------------------------------------------------------
# summary.R
# ------------------------------------------------------------------------------
# 
summary.fitLTMLE <- function(obj, type="diff", conf.int=0.95){
  if(type=="diff"){
    x <- obj$est.diff
    out <- data.frame("est" = c(x$est.ide, x$est.iie, x$est.oe, x$est.gide),
                      "se" = c(x$se.ide, x$se.iie, x$se.oe, x$se.gide),
                      "CI.low" = c(x$est.ide-qnorm((1+conf.int)/2)*x$se.ide, 
                                   x$est.iie-qnorm((1+conf.int)/2)*x$se.iie, 
                                   x$est.oe-qnorm((1+conf.int)/2)*x$se.oe,
                                   x$est.gide-qnorm((1+conf.int)/2)*x$se.gide),
                      "CI.up" = c(x$est.ide+qnorm((1+conf.int)/2)*x$se.ide, 
                                  x$est.iie+qnorm((1+conf.int)/2)*x$se.iie, 
                                  x$est.oe+qnorm((1+conf.int)/2)*x$se.oe,
                                  x$est.gide+qnorm((1+conf.int)/2)*x$se.gide))
    rownames(out) <- c("ide", "iie", "oe", "gide")
  }
  if(type=="OR"){
    x <- obj$est.OR
    out <- data.frame("est" = c(x$est.ORide, x$est.ORiie, x$est.ORoe, x$est.ORgide),
                      "se" = c(x$se.ORide, x$se.ORiie, x$se.ORoe, x$se.ORgide),
                      "CI.low" = c(x$est.ORide-qnorm((1+conf.int)/2)*x$se.ORide, 
                                   x$est.ORiie-qnorm((1+conf.int)/2)*x$se.ORiie, 
                                   x$est.ORoe-qnorm((1+conf.int)/2)*x$se.ORoe,
                                   x$est.ORgide-qnorm((1+conf.int)/2)*x$se.ORgide),
                      "CI.up" = c(x$est.ORide+qnorm((1+conf.int)/2)*x$se.ORide, 
                                  x$est.ORiie+qnorm((1+conf.int)/2)*x$se.ORiie, 
                                  x$est.ORoe+qnorm((1+conf.int)/2)*x$se.ORoe,
                                  x$est.ORgide+qnorm((1+conf.int)/2)*x$se.ORgide))
    rownames(out) <- c("OR_ide", "OR_iie", "OR_oe", "OR_gide")
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
#-------------------------------------------------------------------------------