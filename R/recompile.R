recompile <- function(object) UseMethod("recompile")


recompile.rjags <- function(object){
  object$model$recompile()
  object$model$update(1)
}
  
