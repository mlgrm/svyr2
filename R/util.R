node <- function(x)attr(x,"node")
name <- function(x)attr(x,"node")$name
type <- function(x)attr(x,"node")$type
label <- function(x)ifelse(is.list(node(x)$label),
  node(x)$label[[getOption("svyLang","English")]],
  ifelse(is.null(node(x)$label),"",node(x)$label))
selected <- function(x)attr(x,"selected")
group <- function(x)attr(x,"group")
data <- function(x)attr(x,"data")
choices <- function(x)sapply(node(x)$children,getElement,"name")


data <- function(x) UseMethod("data", x)
data.svr <- function(r,...) do.call(rbind,lapply(1:length(r), function(i){
  df <- as.data.frame(r[[i]],...)
  df <- cbind(ind=rep(i,nrow(df)),df)
}))
data.svq <- identity
data.svg <- function(x)attr(x,"data")
data.svy <- function(x)attr(x,"data")
