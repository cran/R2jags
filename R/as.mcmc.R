as.mcmc.rjags <- function (x) {
  x <- x$BUGSoutput
  if (x$n.chains > 1) {
    z <- list()
    for (i in 1:x$n.chains) {
      z[[i]] <- mcmc(x$sims.array[, i, ], start = 1, thin = x$n.thin)
    }
  class(z) <- "mcmc.list"
  }
  else {
    z <- mcmc(x$sims.matrix, start = 1, thin = x$n.thin)
  }
  return(z)
}
