library(targets)
setwd("/Users/mariebreum/Documents/GitHub/NASH-mediation")
x <- tar_read(res_n4000_bins)
x <- x[bins==20,] 

mean(x$sde)
mean(x$sde - x$sde.true)
sd(x$sde);mean(sqrt(x$sdevar))
mean(x$sde.true > x$sde - qnorm(0.975)*sqrt(x$sdevar) & x$sde.true < x$sde + qnorm(0.975)*sqrt(x$sdevar))

mean(x$sie)
mean(x$sie-x$sie.true)
sd(x$sie);mean(sqrt(x$sievar))
mean(x$sie.true > x$sie - qnorm(0.975)*sqrt(x$sievar) & x$sie.true < x$sie + qnorm(0.975)*sqrt(x$sievar))

mean(x$oe)
mean(x$oe-x$oe.true)
sd(x$oe);mean(sqrt(x$oevar))
mean(x$oe.true > x$oe - qnorm(0.975)*sqrt(x$oevar) & x$oe.true < x$oe + qnorm(0.975)*sqrt(x$oevar))


xmean(x$sde.prop)
mean(x$sde.prop - x$sde.prop.true)
sd(x$sde.prop);mean(sqrt(x$sde.prop.var))
mean(x$sde.prop.true > x$sde.prop - qnorm(0.975)*x$sde.prop.se & x$sde.prop.true < x$sde.prop + qnorm(0.975)*x$sde.prop.se)

mean(x$sie.prop)
mean(x$sie.prop - x$sie.prop.true)
sd(x$sie.prop);mean(sqrt(x$sie.prop.var))
mean(x$sie.prop.true > x$sie.prop - qnorm(0.975)*x$sie.prop.se & x$sie.prop.true < x$sie.prop + qnorm(0.975)*x$sie.prop.se)



mean(x$psi11)
mean(x$psi11-x$psi11.true)
sd(x$psi11);mean(sqrt(x$psi11var))
mean(x$psi11.true > x$psi11 - qnorm(0.975)*sqrt(x$psi11var) & x$psi11.true < x$psi11 + qnorm(0.975)*sqrt(x$psi11var))

mean(x$psi10)
mean(x$psi10-x$psi10.true)
sd(x$psi10);mean(sqrt(x$psi10var))
mean(x$psi10.true > x$psi10 - qnorm(0.975)*sqrt(x$psi10var) & x$psi10.true < x$psi10 + qnorm(0.975)*sqrt(x$psi10var))

mean(x$psi00)
mean(x$psi00-x$psi00.true)
sd(x$psi00);mean(sqrt(x$psi00var))
mean(x$psi00.true > x$psi00 - qnorm(0.975)*sqrt(x$psi00var) & x$psi00.true < x$psi00 + qnorm(0.975)*sqrt(x$psi00var))

