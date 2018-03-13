#' tools for summarizing data sets
library(plyr)
library(dplyr)


summary <- function(x,...) UseMethod("summary", x)

summary.svy <- function(x,...)ldply(flatten(s),summary,...)

summary.svq <- function(x,...){
  if(type(x) %in% names(summary_svq)) summary_svq[[make.names(type(x))]](x) else
    summary_svq$text(x)
}

summary_svq <- list()

summary_svq$text <- function(x,...){
  data.frame(
    name=name(x),
    type=type(x),
    label=label(x),
    summary=sprintf("%d unique values",length(unique(x))),
    group=paste(group(x),collapse = "/")
  )
}

num_types <- c(
  "dateTime",
  "date",
  "integer",
  "decimal",
  "start",
  "end",
  "today"
)

num_types_sum <- function(x)
  paste("quartiles",paste(summary(x),collapse = ",",sep=": "))

summary_svq <- c(summary_svq,sapply(num_types,function(x)num_types_sum))

summary_svq$geopoint <- function(x)
  sprintf("centroid: %s",
          paste(colnames(x),
                sprintf(c("%0.4f","%0.4f","%0.0f","%0.0f"),
                        c(
                          mean(x[,1],na.rm=TRUE),
                          mean(x[,2],na.rm=TRUE),
                          mean(x[,3],na.rm=TRUE),
                          sqrt(sum(x[,4]^2,na.rm=TRUE)/length(x[!is.na(x)]))
                        )
                ), sep="=",collapse=","
          )
  )

summary_svq$`select one`
