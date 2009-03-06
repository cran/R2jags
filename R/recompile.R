recompile <- function(object, n.iter) UseMethod("recompile")


recompile.rjags <- function(object, n.iter=100){
  object$model$recompile()
  update(object$model, n.iter=n.iter) 
}
  
