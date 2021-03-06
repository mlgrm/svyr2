library(jsonview)


svy <- function(form=get.kobo.form(),data=get.kobo.data()){
  # extracting a survey is just like extracting a roster
  if(is.null(data))browser()
  s <- get.data(form,NULL,data)
  s
}

#' wrapper function for the functions in the list "extract"
get.data <- function(node,group,data){
  type <- make.names(node$type)
  datum <- if(type %in% names(extract)) extract[[type]](node,group,data) else
    extract$text(node,group,data)
  attr(datum,"node") <- node
  attr(datum,"group") <- group
  if(!any(grepl("^sv[rgy]",class(datum)))) class(datum) <- c("svq",class(datum))
  datum
}

#' named list of functions to retrieve the data for each question type in odk
extract <- list()

#' extract a whole survey or a mini-survey (e.g. household roster)
extract$survey <- function(node,group,data){
  s <- lapply(node$children,get.data,group=group,data=data)
  names(s) <- sapply(node$children,getElement,"name")
  class(s) <- c("svy",class(s))
  attr(s,"data") <- data
  s
}

#' the default type and underlying retrieval function for all other atomic
#' types
extract$text <- function(node,group,data){
  field.name <- paste(c(group,node$name),collapse="/")
  if(is.data.frame(data)) 
    dat <- as.character(data[[field.name]]) else
      dat <- sapply(data,function(r){
        val <- paste(as.character(r[[field.name]]),collapse=", ")
        if(val=="") val <- NA_character_
        val
      })
  # class(dat) <- c("svq",class(dat))
  dat
}

#' wrapper for retrieving integer data
extract$integer <- function(node,group,data){
  as.integer(extract$text(node,group,data))
}

#' wrapper for retrieving numeric data
extract$decimal <- function(node,group,data){
  as.numeric(extract$text(node,group,data))
}

#' wrapper for retrieving gps coordinates
extract$geopoint <- function(node,group,data){
  m <- do.call(rbind, strsplit(extract$text(node,group,data),' '))
  class(m) <- "numeric"
  colnames(m) <- c("latitude","longitude","altitude","precision")
  m
}

#' wrapper for retrieving dates
extract$date <- function(node,group,data){
  as.Date(extract$text(node,group,data))
}

extract$today <- extract$date

#' wrapper for retrieving date/times (uses mrdwab's parser)
extract$dateTime <- function(node,group,data){
  kobo_time_parser_UTC(extract$text(node,group,data))
}

extract$start <- extract$end <- extract$dateTime

extract$note <- function(node,group,data){
  rep('',length(data))
}
#' retrieves "select one" data as a factor with correct labels
extract$select.one <- function(node,group,data){
  r <- extract$text(node,group,data)
  lang <- getOption("svyLang","default")
  if(lang=="default") lang <- names(node$label)[1] else
    if(!(lang %in% names(node$label)))
      stop(paste("language", lang, "not available in node", node$name))
  lvl <- make.unique(sapply(node$children,getElement,"name"))
  r <- factor(r,levels=lvl)
  if(length(node$children[[1]]$label)==1)
    attr(r,"labels") <- sapply(node$children,getElement,"label") else
      attr(r,"labels") <- sapply(node$children,
                                 function(ch)ch$label[[lang]][[1]])
  r
}

#' retrieves "select all that apply" data as a logical matrix with correct labels
#' and colnames
extract$select.all.that.apply <- function(node,group,data){
  # browser(expr=(node$name=="migration"))
  x <- extract$text(node,group,data)
  lang <- getOption("svyLang","default")
  if(lang=="default") lang <- names(node$label)[1]
  x <- lapply(strsplit(x,' '),function(r){
    is.element(seq_len(length(node$children)),r)
  })
  x <- do.call(rbind,x)

  colnames(x) <- make.unique(sapply(node$children,getElement,"name"))
  if(length(node$children[[1]]$label)==1)
    attr(x,"labels") <- sapply(node$children,getElement,"label") else
      attr(x,"labels") <- sapply(node$children,
                                 function(ch)ch$label[[lang]][[1]])
  x
}

#' extracts all the data in a group recursively, with special behaviour for
#' repeats.
extract$group <- function(node,group,data){
  group <- c(group,node$name)
  df <- lapply(node$children,get.data,group=group,data=data)
  names(df) <- sapply(node$children,getElement,"name")
  class(df) <- c("svg",class(df))
  df
}
# extract$group <- fun2

#' extracts all the data in a repeat group as a separate "survey" for each
#' respondent
extract$repeat. <- function(node,group,data){ # repeat is a reserved word
  # this is a node of type repeat which has one child for every question asked
  # to every family member.  we return a list of surveys, one for each household
  # the function applies extract$survey on each data set using the same node
  # this is a kind of group so push the name onto group
  group <- c(group,node$name)
  datl <- lapply(data,getElement,paste0(group,collapse='/'))
  names(datl) <- 1:length(datl)
  node$type <- "survey"
  l <- lapply(datl,function(d)get.data(node,group,d))
  class(l) <- c("svr", class(l))
  l
}
# extract$repeat. <- fun1




# these two functions borrowed from:
# https://github.com/mrdwab/koboloadeR

kobo_time_parser <- function(instring, timezone = Sys.timezone()) {
  format(kobo_time_parser_UTC(instring), tz = timezone, usetz = TRUE)
}

kobo_time_parser_UTC <- function(instring) {
  tmp <- gsub("\\.\\d{3}|:", "", instring)
  tmp <- chartr(" ", "0", format(tmp, justify = "left", width = 22))
  as.POSIXct(strptime(tmp, format = "%Y-%m-%dT%H%M%S%z", tz = "UTC"))
}
