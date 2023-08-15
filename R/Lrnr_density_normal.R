Lrnr_density_normal <- R6::R6Class(
  classname = "Lrnr_density_normal",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous"),
    .train = function(task) {
      args <- self$params
      
      args$x <- as.matrix(task$X)
      args$y <- task$Y
      
      lm_fit <- sl3:::call_with_args(stats::lm.fit, args)

      fit_object <- list(
        fit = lm_fit,
        coef = lm_fit$coefficients,
        sd = sd(task$Y)
      )

      return(fit_object)
    },
    .predict = function(task) {
      verbose <- getOption("sl3.verbose")
      
      X <- as.matrix(task$X)
      Y <- task$Y
      
      coef <- self$fit_object$coef
      X <- as.matrix(X)
      mu <- X %*% coef
      sd <- self$fit_object$sd
      
      predictions <- dnorm(Y, mean = mu, sd = sd)
      
      return(predictions)
    }
  )
)

