library(plyr)
library(dplyr)

node <- function(x)attr(x,"node")
name <- function(x,use.node=TRUE){
  if(use.node) x <- node(x)
  x$name
}
type <- function(x,use.node=TRUE){
  if(use.node) x <- node(x)
  x$type
}
label <- function(x,use.node=TRUE){
  if(use.node) node <- node(x) else node <- x
  lbl <- ifelse(is.list(node$label),
                node$label[[getOption("svyLang","English")]],
                ifelse(is.null(node$label),"",node$label))
  # we can only get the question and answer if we got the whole datum
  if(use.node && type(x)=="select all that apply")
    lbl <- paste(lbl,labels(x)[selected(x)],sep=":")
  lbl
}

preserve <- function(x,...) UseMethod("preserve", x)

preserve.list <- function(
  l, fun,
  incl=getOption("svyAttrIncl",names(attributes(x))),
  incl.list=getOption("svyAttrInclList", c(
    "node",
    "data"
  )),...){
  l0 <- fun(l)
  for(i in 1:length(l)) attributes(l0[[i]])[incl] <- attributes(l[[i]])[incl]
  attributes(l0)[incl.list] <- attributes(l)[incl.list]
  if(class(l)[1]!=class(l0)[1]) class(l0) <- c(class(l)[1],class(l0))
  l0
}
preserve.default <- function(
  x,fun,incl=getOption("svyAttrIncl",names(attributes(x))),...){
  x0 <- fun(x,...)
  attributes(x0)[incl] <- attributes(x)[incl]
  x0
}

labels <- function(x,use.node=TRUE){
  if(use.node) node <- node(x) else node <- x
  lbls <- sapply(node$children,label,use.node=FALSE)
  names(lbls) <- sapply(node$children,name,use.node=FALSE)
  lbls
}

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
  l <- lapply(names(x),function(n){
    df <- as.data.frame(x[[n]])
    # if(is(tryCatch(rep(n,nrow(df)),error=identity),"error"))browser()
    df <- cbind(id=rep(n,nrow(df)),df)
  })
  do.call(rbind,l)
}
data.svq <- identity
data.svg <- function(x)attr(x,"data")
data.svy <- function(x)attr(x,"data")
