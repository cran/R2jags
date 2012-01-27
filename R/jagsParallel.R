
jags.parallel <- function (data, inits, parameters.to.save, model.file = "model.bug",
                           n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2),
                           n.thin = max(1, floor((n.iter - n.burnin)/1000)),
                           n.cluster= n.chains, DIC = TRUE,
                           working.directory = NULL, jags.seed = 123 )
{
  jags.params <- parameters.to.save
  jags.inits  <- if(missing(inits)){ NULL} else{inits}
  jags.model  <- model.file
  
  .runjags <- function() {
    #library(R2jags)
    jagsfit <- jags(data               = eval(expression(data)),
                    inits              = jags.inits,
                    parameters.to.save = eval(expression(jags.params)), 
                    model.file         = eval(expression(jags.model)),
                    n.chains           = 1,
                    n.iter             = eval(expression(n.iter)),
                    n.burnin           = eval(expression(n.burnin)),
                    n.thin             = eval(expression(n.thin)),
                    DIC                = eval(expression(DIC)),
                    working.directory  = eval(expression(working.directory)),
                    jags.seed          = eval(expression(jags.seed)),
                    progress.bar       = "none"
                    )
    return(jagsfit)
  }

  cl <- makeCluster( n.cluster, methods = FALSE )
  clusterExport(cl, data)
  clusterSetRNGStream(cl, jags.seed)
  tryCatch( res <- clusterCall(cl,.runjags), finally = stopCluster(cl) )
  #adim    <- dim( res[[1]]$BUGSoutput$sims.array )
  result  <- NULL
  model <- NULL
  for( ch in 1:n.chains ){
     result <- abind(result, res[[ch]]$BUGSoutput$sims.array, along=2)
     model[[ch]] <- res[[ch]]$model
  }
  result <- as.bugs.array2(result, model.file=model.file, program="jags", DIC=DIC, 
      n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
  #res[[1]]$model$nchain <- n.chains
  out <- list(model = model, BUGSoutput = result, parameters.to.save = parameters.to.save, 
      model.file = model.file, n.iter = n.iter, DIC = DIC) 
  class(out) <- c("rjags.parallel", "rjags")
  return(out)
}
