make.sql.names <- function(df,max.len=64){
  ns <- sapply(df,function(x){
    n <- name(x)
    if(type(x)=="select all that apply") n <- paste(n,selected(x),sep="/")
    n
  })
  ns <- substr(ns,1,max.len-8)
  make.unique(ns)
}