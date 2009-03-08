\name{recompile}
\alias{recompile}
\alias{recompile.rjags}


\title{Function for recompiling rjags object}
\description{
The \code{recompile} takes a \code{rjags} object as input.  
\code{recompile} will re-compile the previous saved \code{rjags} object.
}

\usage{

recompile(object, n.iter, refresh)
\method{recompile}{rjags}(object, n.iter=100, refresh=n.iter/50)


}


\arguments{
  \item{object}{an object of \code{rjags} class.}
  \item{n.iter}{number of iteration for adapting, default is 100}
  \item{refresh}{refresh rate of the progress bar, default is \code{n.iter/100}}
}


\author{ 
  Yu-Sung Su \email{ys463@columbia.edu}
}


\examples{
 # see ?jags for an example.
}

\keyword{models}
