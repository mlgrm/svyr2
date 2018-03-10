make.sql.names <- function(df,max.len=63){
  ns <- sapply(df,function(x){
    n <- name(x)
    if(type(x)=="select all that apply") n <- paste(n,selected(x),sep="_")
    n
  })
  ns <- tolower(ns)
  ns <- ifelse(substr(ns,nchar(ns),nchar(ns))=="_" |
           substr(ns,1,1)=="_" |
           grepl("[0-9]",ns), ns, paste0(ns,"_"))
  ns <- ifelse(grepl("^[0-9]",ns),paste0("_",ns),ns)
  ns <- gsub("[^A-z0-9_]+","_",ns)
  i <- 0
  repeat{
    ns <- substr(ns,1,max.len-i)
    ns <- make.unique(ns,sep = "_")
    if(max(sapply(ns,nchar)<=max.len)) break
    i <- i+1
  }
  cat(i,"\n")
  ns
}
