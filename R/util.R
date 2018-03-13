library(dplyr)

node <- function(x)attr(x,"node")
name <- function(x)attr(x,"node")$name
type <- function(x)attr(x,"node")$type
label <- function(x,use.node=TRUE){
  if(use.node) node <- node(x) else node <- x
  ifelse(is.list(node$label),
  node$label[[getOption("svyLang","English")]],
  ifelse(is.null(node$label),"",node$label))
}
labels <- function(x)sapply(node(x)$children,label,use.node=FALSE)

selected <- function(x)attr(x,"selected")
group <- function(x)attr(x,"group")
data <- function(x)attr(x,"data")

choices <- function(x,...) UseMethod("choices",x)
choices.svq <- function(x,...)sapply(node(x)$children,getElement,"name")
choices.svy <- function(x,...){
  # browser()
  x <- flatten(x)
  x <- x[sapply(x,type) %in% c("select one","select all that apply")]
  ldply(x,function(q){
    data.frame(
      choice=choices(q,...),
      count=counts[[make.names(type(q))]](q),
      label=labels(q))
  },.id="qid")
}

counts <- list(
  select.one=function(x)as.integer(table(x)),
  select.all.that.apply=function(x)colSums(x,na.rm=TRUE)
)

data <- function(x) UseMethod("data", x)
data.svr <- function(x,...){
  ldply(x, function(df){
    df <- as.data.frame(df,...)
    df
  },.id="id")
}
data.svq <- identity
data.svg <- function(x)attr(x,"data")
data.svy <- function(x)attr(x,"data")
