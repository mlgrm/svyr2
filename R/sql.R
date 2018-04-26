library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgres)

connection <- function(host=getOption("svyDBHost"),
                    user=getOption("svyDBUser"),
                    password=getOption("svyDBPassword"),
                    dbname=getOption("svyDBName")){
  if(!("svyDBConnection" %in% names(options())))
    options(svyDBConnection=dbConnect(Postgres(),
                                      host=host,
                                      user=user,
                                      password=password,
                                      dbname=dbname
    ))
  getOption("svyDBConnection")
}

doSQL <- function(statement,...)
  dbClearResult(
    dbSendQuery(connection(),statement, ...)
  )


push <- function(s,name=deparse(substitute(df),...),
                 schema=getOption("svyDBSchema",NULL),
                 indexes=NULL,
                 overwrite=FALSE,
                 make.map=true, ...){
  if(is.svy){

  }
  s <- flatten(s)
  db.names <- make.sql.names(names(s))
  if(make.map)
    map <- data.frame(name=names(df),
                    db.name=db.names,
                    type=sapply(df,type),
                    label=sapply(df,label))
  names(df) <- db.names
  # connect(...)
  con <- connection()
  suppressWarnings(doSQL(paste("create schema if not exists",
                               getOption("svyDBSchema"))))
  doSQL(paste("set search_path to",getOption("svyDBSchema")))
  copy_to(con,df,
          name=name,
          temporary=FALSE,
          overwrite=overwrite,
          indexes=indexes
  )
  if(make.map) copy_to(con,map,
          name=paste(name,"map",sep="_"),
          temporary=FALSE,
          overwrite=overwrite,
          indexes=list("db.name")
  )
  t <- tbl(con,name)
  m <- tbl(con,paste(name,"map",sep="_"))
  #dbDisconnect(getOption("svyDBConnection"))
  invisible(list(data=t,map=m))
}

# raw applies to plain data.frames, so the attributes are not checked
make.sql.names <- function(
  ns, max.len=63,raw=FALSE,remove.group=TRUE,group.delimiter=c("/",".")){
  if(remove.group) ns <- sub("^.*/","",ns)
  ns <- tolower(ns)
  ns <- ifelse(ns %in% tolower(.SQL92Keywords), paste0(ns,"_"),ns)
  ns <- gsub("[^A-z0-9_]+","_",ns)
  i <- 0
  repeat{
    ns <- substr(ns,1,max.len-i)
    ns <- make.unique(ns,sep = "_")
    if(max(sapply(ns,nchar)<=max.len)) break
    i <- i+1
  }
  # cat(i,"\n")
  ns
}

