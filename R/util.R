node <- function(x)attr(x,"node")
name <- function(x)attr(x,"node")$name
type <- function(x)attr(x,"node")$type
label <- function(x)if(is.list(node(x)$label)) node(x)$label[[getOption("svyLang","English")]] else node(x)$label
selected <- function(x)attr(x,"selected")
group <- function(x)attr(x,"group")
data <- function(x)attr(x,"data")
