recompile <- function(object, n.iter, refresh) UseMethod("recompile")


recompile.rjags <- function(object, n.iter=100, refresh=n.iter/50){
  object$model$recompile()
  adapt(object$model, n.iter=n.iter, by=refresh) 
}
  
