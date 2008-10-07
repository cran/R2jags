autojags <- function(object, n.iter=1000, n.thin=1, Rhat=1.1, n.update=2, ...){
  if(class(object)!="rjags") stop("model must be a rjags object")
    fit <- update(object, n.iter=n.iter, n.thin=n.thin, ...)
    check <- ifelse(max(fit$BUGSoutput$summary[,"Rhat"]) > Rhat, 1, 0)
    if (check > 0){
      count <- 1
      while (check > 0 & n.update >= count) {
          fit <- update(fit, n.iter=n.iter, n.thin=n.thin, ...)
          count <- count + 1
          check <- ifelse(max(fit$BUGSoutput$summary[,"Rhat"]) > Rhat, 1, 0)
      }
    }
    fit
}
