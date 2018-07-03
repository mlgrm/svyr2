flatten <- function(x) UseMethod("flatten",x)
flatten.svq <- list
flatten.svg <- function(x)do.call(c,lapply(x,flatten))
flatten.svy <- flatten.svg
flatten.svr <- list

as.data.frame <- function(x,...) UseMethod("as.data.frame",x)

as.data.frame.svy <- function(s, labels=TRUE){
  s <- flatten(s)
  s1 <- lapply(s,function(e){
    # cat(class(e),"\n")
    as.data.frame(e)
  })
  browser(expr=is(tryCatch(as.data.frame.list(s1),error=identity),"error"))
  as.data.frame.list(s1)
}

# as.data.frame.svy <- function(s,simplify=TRUE,odk.names=TRUE,...){
#   s <- flatten(s)
#   node <- lapply(s,attr,"node")
#   # find protected matrices and flatten them
#   # s <- lapply(s,as.data.frame)
#   s <- mapply(function(e,a){
#     # browser(expr=(any(sapply(e,length)==0)))
#     attr(e,"node") <- a
#     e <- as.data.frame(e, stringsAsFactors=FALSE)
#     e
#   }, s, node, SIMPLIFY = FALSE)
#   s <- s[!sapply(s,is.null)]
#   if(length(s)==0) return(NULL)
#   browser(expr=is(tryCatch(do.call(cbind,s),error=identity),"error"))
#   df <- do.call(cbind,s)
#   if(odk.names)
#     names(df) <- sapply(df,function(c){
#       n <- name(c)
#       if(type(c)=="select all that apply") n <- paste(n,selected(c),sep=":")
#       if(length(group(c))>0) n <- c(group(c),n)
#       n <- paste(n,collapse = "/")
#       n
#     })
#   df
# }

as.data.frame.svq <- function(x,...){
  # if(type(x)=="note"){
  #   return()
  # }
  if(is.matrix(x)){
    df <- as.data.frame(lapply(colnames(x),function(n){
      col <- x[,n]
      attr(col,"node") <- node(x)
      attr(col,"node")$name <- paste(name(x),n,sep = ":")
      col
    }),...)
  } else {
    # all other types should be vectors
    # remove the svq class
    class(x) <- class(x)[class(x)!='svq']
    # turn the vector into a one-column data.frame for cbinding
    df <- data.frame(x)
    browser(expr=(length(df)!=1))
    names(df) <- name(x)
  }
  browser(expr=any(grepl("\\.NA$",names(df))))
  df
}


as.data.frame.svg <- as.data.frame.svy

as.data.frame.svr <- function(r, simplify=FALSE, ...){
  ind <- 1:length(r)
  attr(ind,"node") <- attr(r,"node")

  # the rbound dfs are now the data attribute of the svr
  attr(ind,"data") <- data(r)
  data.frame(ind,...)
}
