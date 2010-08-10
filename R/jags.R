jags <- function (data, inits, parameters.to.save, model.file = "model.bug", 
  n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
  n.thin = max(1, floor((n.iter - n.burnin)/1000)),
  DIC = TRUE, working.directory = NULL, 
  refresh = n.iter/50, progress.bar="text") 
{  
  require(rjags)
  if (!is.null(working.directory)) {
    working.directory <- path.expand(working.directory)
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
  }
  else {
    saveWD <- getwd()
    working.directory <- saveWD
  }

  ## jags.model() needs 'data' to be "a list or environment containing the data"
  if (is.character(data) && length(data)==1 && regexpr("\\.txt$",data)>0) {
    ## 1. 'data' looks like a file name [UNDOCUMENTED!]
    if (all(basename(data) == data)) {
      try(file.copy(file.path(savedWD, data), data, overwrite = TRUE))
    }
    if (!file.exists(data)) {
      stop("File",data,"does not exist")
    }
    e <- new.env()
    eval(parse(data), e)
    data <- as.list(e)
  } else if (is.character(data) || (is.list(data) && all(sapply(data,is.character)))) {
    ## 2. data is a character vector or a list of character
    dlist <- lapply(as.list(data), get, envir=parent.frame(1))
    names(dlist) <- unlist(data)
    data <- dlist
  } else if (!is.list(data)) {
    stop("data must be a character vector of object names, a list of object names, or a list of objects")
  }
                                                                  
  if (DIC){
    parameters.to.save <- c(parameters.to.save, "deviance")
    load.module("dic", quiet = TRUE)    
  }
  
  if(is.null(inits)){
    m <- jags.model(model.file, data = data, n.chains = n.chains, 
      n.adapt = 0)
    if(n.burnin>0){
      n.adapt <- n.burnin
    }
    if(n.burnin==0){
      n.adapt <- 100
    }
    adapt(m, n.iter = n.adapt, by = refresh, progress.bar = progress.bar) 
  }
  else{ 
    m <- jags.model(model.file, data = data, inits=inits, n.chains = n.chains, 
      n.adapt = 0)
    if(n.burnin>0){
      n.adapt <- n.burnin
    }
    if(n.burnin==0){
      n.adapt <- 100
    }
    adapt(m, n.iter = n.adapt, by = refresh, progress.bar = progress.bar) 
  }
  
  
  samples <- coda.samples(model = m, variable.names = parameters.to.save, 
      n.iter = (n.iter-n.burnin), thin = n.thin, by = refresh, 
      progress.bar = progress.bar)
  
  fit <- mcmc2bugs(samples, model.file = model.file, program = "jags", 
      DIC = DIC, DICOutput = NULL, n.iter = n.iter, n.burnin = n.burnin, 
      n.thin = n.thin)
  
  out <- list(model=m, BUGSoutput = fit, parameters.to.save = parameters.to.save, 
      model.file = model.file, n.iter = n.iter, DIC = DIC)
  
  
  class(out) <- "rjags"
  return(out)
}
