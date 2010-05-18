\name{jags}
\docType{class}
% Classes
\alias{rjags-class}
% functions
\alias{jags}
\alias{jags2}


\title{Run jags from R}
\description{
  The \code{jags} function takes data and starting values as input.  It
  automatically writes a \code{jags} script, calls the model, and
  saves the simulations for easy access in R.
}

\usage{
jags(data, inits, parameters.to.save, model.file="model.bug",
  n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
  n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
  DIC=TRUE, working.directory=NULL,
  refresh = n.iter/50, progress.bar = "text")


jags2(data, inits, parameters.to.save, model.file="model.bug",
  n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
  n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
  DIC=TRUE, jags.path="", 
  working.directory=NULL, clearWD=TRUE,
  refresh = n.iter/50)
}


\arguments{
  \item{data}{a vector or list of the names of the data objects used by
    the model, or a list of the data objects themselves.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is 
    \code{NULL}, \pkg{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored.}
  \item{model.file}{file containing the model written in \code{BUGS} code.}
  \item{n.chains}{number of Markov chains (default: 3)}
  \item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations. If n.burnin is 0,
    \code{jags()} will run 100 iterations for adaption.}
  \item{n.thin}{thinning rate. Must be a positive integer.  Set
    \code{n.thin} > 1 to save memory and computation time if
    \code{n.iter} is large.  Default is \code{max(1, floor(n.chains *
    (n.iter-n.burnin) / 1000))} which will only thin if there are at
    least 2000 simulations.}
%  \item{n.sims}{The approximate number of simulations to keep after thinning.}
%  \item{n.adapt}{the number of iterations for adaptation.  When a model is
%          first created, some of the samplers may have an adaptive
%          mode. In this case, the samplers are run for \code{n.adapt}
%          iterations and then the adaptive mode is switched off.}
  \item{DIC}{logical; if \code{TRUE} (default), compute deviance, pD,
    and DIC. The rule \code{pD=var(deviance) / 2} is used.}
  \item{working.directory}{sets working directory during execution of
    this function; This should be the directory where model file is.}
  \item{jags.path}{directory that contains the \pkg{jags} executable.
    The default is \dQuote{}.  }
  \item{clearWD}{indicating whether the files \file{data.txt},
    \file{inits[1:n.chains].txt}, \file{codaIndex.txt}, \file{jagsscript.txt},
    and \file{CODAchain[1:nchains].txt} should be removed after \pkg{jags} has
    finished, default=TRUE.}
  \item{refresh}{refresh frequency for progress bar, default is \code{n.iter/50}}
  \item{progress.bar}{type of progress bar. Possible values are \dQuote{text}, 
      \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is displayed
      on the R console. Type \dQuote{gui} is a graphical progress bar 
      in a new window. The progress bar is suppressed if \code{progress.bar} is 
      \dQuote{none}}
}



\details{
  To run:
  \enumerate{
    \item Write a \pkg{BUGS} model in an ASCII file.
    \item Go into \R.
    \item Prepare the inputs for the \code{jags} function and run it (see
      Example section).
    \item The model will now run in \pkg{JAGS}. It might take awhile. You
      will see things happening in the R console.
  }

  BUGS version support:
  \itemize{
    \item{\pkg{jags} 1.0.3}{default}
  }
}

\author{ 
  Yu-Sung Su \email{ys463@columbia.edu},
  Masanao Yajima \email{yajima@stat.columbia.edu}
}


\references{   
    Plummer, Martyn (2003) 
    \dQuote{JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling.}
    \url{http://citeseer.ist.psu.edu/plummer03jags.html}.
    
    Gelman, A., Carlin, J. B., Stern, H.S., Rubin, D.B. (2003)
    \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.

    Sibylle Sturtz and Uwe Ligges and Andrew Gelman. (2005). 
      \dQuote{R2WinBUGS: A Package for Running WinBUGS from R.}
      \emph{Journal of Statistical Software} 3 (12): 1--6.
}




\examples{
  # An example model file is given in:
  model.file <- system.file(package="R2jags", "model", "schools.txt")
  # Let's take a look:
  file.show(model.file)

  # data
  J <- 8.0
  y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
  sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)

  jags.data <- list("y","sd","J")
  jags.params <- c("mu","sigma","theta")
  jags.inits <- function(){
    list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
  }
  
#=============#
# using jags  #
#=============#
  jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
    n.iter=5000, model.file=model.file)
    
  # display the output
  print(jagsfit)
  plot(jagsfit)
  
  # traceplot
  traceplot(jagsfit)
  
  # or to use some plots in coda
  # use as.mcmmc to convert rjags object into mcmc.list
  jagsfit.mcmc <- as.mcmc(jagsfit)
  ## now we can use the plotting methods from coda
  xyplot(jagsfit.mcmc)
  densityplot(jagsfit.mcmc)
  
  # if the model does not converge, update it!
  jagsfit.upd <- update(jagsfit, n.iter=1000)
  print(jagsfit.upd)
  plot(jagsfit.upd)
  
  # or auto update it until it converges! see ?autojags for details
  jagsfit.upd <- autojags(jagsfit)
    
  # to get DIC or specify DIC=TRUE in jags() or do the following#
  ### dic.samples(jagsfit.upd$model, n.iter=1000, type="pD") not working!!
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.upd)
  
  # this will show a 3-way array of the bugs.sim object, for example:
  mu
  
  # detach jags object into search path see "attach.bugs" for details
  detach.jags()

  # to pick up the last save session
  # for example, load("RWorkspace.Rdata")
  recompile(jagsfit)
  jagsfit.upd <- update(jagsfit)

  
#=============#
# using jags2 #
#=============#
  ## jags cannot be updated, but produces coda files
  ## You may need to edit "jags.path",
  ## also you need write access in the working directory:
  ## currently works only under Windows OS
  ## e.g. setwd("c:/")

  ## NOT RUN HERE
  jagsfit <- jags2(data=jags.data, inits=jags.inits, jags.params, 
    n.iter=5000, model.file=model.file)
  print(jagsfit)
  plot(jagsfit)
  # or to use some plots in coda
  # use as.mcmmc to convert rjags object into mcmc.list
  jagsfit.mcmc <- as.mcmc.list(jagsfit)
  traceplot(jagsfit.mcmc)
  xyplot(jagsfit.mcmc)
  densityplot(jagsfit.mcmc)

}

\keyword{interface}
\keyword{models}
