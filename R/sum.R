#' tools for summarizing data sets
library(plyr)
library(dplyr)


summary <- function(x,...) UseMethod("summary", x)

summary.svy <- function(x,...)ldply(flatten(s),summary,...)

summary.svq <- function(x,...){
  if(type(x) %in% names(summary_svq)) summary_svq[[type(x)]](x) else summary_svq$text(x)
}

summary_svq <- list()

summary_svq$text <- function(x,...){
  data.frame(
    name=name(x),
    type=type(x),
    label=label(x),
    summary=sprintf("%s unique values",length(unique(x)))
  )
}
