update.rjags <- function(object, n.iter=1000, n.thin=1, refresh=n.iter/50,...)
{
  samples <- coda.samples(object$model, variable.names=object$parameters.to.save, n.iter=n.iter, thin = n.thin, by=refresh)
  fit <- mcmc2bugs(samples, model.file = object$model.file, program = "jags", DIC = object$DIC, #DICOutput = NULL, 
                    n.iter = n.iter, n.burnin = object$n.iter, n.thin = n.thin)
  out <- list(model=object$model, BUGSoutput=fit, parameters.to.save=object$parameters.to.save,
    model.file = object$model.file, n.iter=n.iter+object$n.iter, DIC = object$DIC)
#  object$model <- object$model$update(niter=n.iter, ...)
  class(out) <- "rjags"
  return(out)
} 
