jags <- function (data, inits, parameters.to.save, model.file = "model.bug", 
  n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
  n.thin = max(1, floor((n.iter - n.burnin)/n.sims)), n.sims = 1000, 
  n.adapt = 1000, DIC = FALSE, working.directory = NULL, 
  refresh = n.iter/50, progress.bar="text") 
{
  
  require(rjags)
  if (!is.null(working.directory)) {
    savedWD <- getwd()
    #working.directory <- win2unixdir(working.directory)
    setwd(working.directory)
    on.exit(setwd(savedWD))
  }
  else {
    working.directory <- getwd()
    #working.directory <- win2unixdir(working.directory)
    setwd(working.directory)
  }
  
  #data.list <- lapply(as.list(data), get, pos = parent.frame(2))
  data.list <- lapply(as.list(data), get, pos = parent.frame(1))
  names(data.list) <- as.list(data)
  lapply(names(data.list), dump, append=TRUE, file="jagsdata.txt",
      envir=parent.frame(1))
  data <- read.jagsdata("jagsdata.txt")
  file.remove("jagsdata.txt")              
  
  if (is.function(model.file)) {
    temp <- tempfile("model")
    temp <- if (is.R() || .Platform$OS.type != "windows") {
      paste(temp, "txt", sep = ".")
    }
    else {
      gsub("\\.tmp$", ".txt", temp)
    }
    write.model(model.file, con = temp)
    model.file <- gsub("\\\\", "/", temp)
    if (!is.R()) 
      on.exit(file.remove(model.file), add = TRUE)
  }   
                                                                  
  if (DIC){parameters.to.save <- c(parameters.to.save, "deviance")}
  
  if(is.null(inits)){
    m <- jags.model(model.file, data = data, n.chains = n.chains, 
      n.adapt = 0)
    adapt(m, n.iter =  n.adapt, by = refresh, progress.bar = progress.bar) 
  }
  else{ 
    m <- jags.model(model.file, data = data, inits=inits, n.chains = n.chains, 
      n.adapt = 0)
    adapt(m, n.iter =  n.adapt, by = refresh, progress.bar = progress.bar) 
  }
  
  if(n.burnin > 0){
    update(m, n.burnin, by = refresh, progress.bar = progress.bar)
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
