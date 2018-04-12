
#' compose two functions (mainly for use in *apply functions)
#' if you need to compose a list of functions, use 'Reduce'
'%*%' <- function(f,g) UseMethod("compose",f)
compose.function <- function(f,g)compose(f,g)
compose <- function(f,g,f.args=list(),g.args=list())
  function(x)do.call(
    f,c(list(
      do.call(g,c(list(x),g.args))),
      f.args))
