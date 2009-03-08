jags2 <- function (data, inits, parameters.to.save, model.file = "model.bug", 
  n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
  n.thin = max(1, floor((n.iter - n.burnin)/n.sims)), n.sims=1000, 
  refresh = n.iter/50, 
  DIC = TRUE, jags.path = "", working.directory = NULL, clearWD = TRUE) 
{
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
  redo <- ceiling(n.iter - n.burnin)

  data.list <- lapply(as.list(data), get, pos = parent.frame(2))
  names(data.list) <- as.list(data)
  lapply(names(data.list), dump, append=TRUE, file="jagsdata.txt")
  data <- read.jagsdata("jagsdata.txt")

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
  jags.call <- if (jags.path == "") {
    "jags"
  }
  else {
    jags.path <- win2unixdir(jags.path)
    paste(jags.path, "jags", sep = "")
  }
  no.inits <- FALSE
  inits.files <- NULL
  if(is.null(inits)){
    no.inits <- TRUE
  } else if (is.function(inits)){
    for (i in 1:n.chains) {
      initial.values <- inits()
      inits.files <- c(inits.files, paste("jagsinits", i, ".txt", sep = ""))
      with(initial.values, dump(names(initial.values), file = paste("jagsinits", i, ".txt", sep = "")))
    }
  } else if (is.list(inits)){
    if (length(inits)==n.chains){
      for (i in 1:n.chains) {
        initial.values <- inits[[i]]
        inits.files <- c(inits.files, paste("jagsinits", i, ".txt", sep = ""))
        with(initial.values, dump(names(initial.values), file = paste("jagsinits", i, ".txt", sep = "")))
      }
    } else {
      stop(message="initial value must be specified for all of chains")
    }
  } 
  
  if (DIC){parameters.to.save <- c(parameters.to.save, "deviance")}

  
  cat("model clear\ndata clear\n", 
      "model in ", "\"", model.file, "\"", "\n", 
      "data in ", "\"jagsdata.txt\"", "\n", 
      "compile, nchains(", n.chains, ")", "\n", 
      if(!no.inits){
      paste("inits in \"", inits.files, "\"\n", sep = "")}, 
      "initialize", "\n", 
      "update ", n.burnin, ", by(", refresh, ")\n", 
      paste("monitor ", parameters.to.save, ", thin(", n.thin, ")\n", sep = ""),      
      "update ", redo, ", by(", refresh, ")\n", 
      "coda *\n", sep = "", file = "jagsscript.txt")
  
  system(paste(jags.call, "jagsscript.txt"))
  
  fit <- jags.sims(parameters.to.save = parameters.to.save, 
      n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, 
      n.thin = n.thin, DIC = DIC)
  
  if (clearWD) {
      file.remove(c("jagsdata.txt", "codaIndex.txt", inits.files, 
          "jagsscript.txt", paste("CODAchain", 1:n.chains, 
              ".txt", sep = "")))
  }
  class(fit) <- "bugs"
  return(fit)
}
