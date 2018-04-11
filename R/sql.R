library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgres)

connect <- function(host=getOption("svyDBHost"),
                    user=getOption("svyDBUser"),
                    password=getOption("svyDBPassword"),
                    dbname=getOption("svyDBName")){
  options(svyDBConnection=dbConnect(Postgres(),
                                    host=host,
                                    user=user,
                                    password=password,
                                    dbname=dbname
                                    )
          )
}

push <- function(df,name=deparse(substitute(df),...),
                 schema=getOption("svyDBSchema",NULL),
                 indexes=NULL,
                 overwrite=FALSE,...){
  map <- data.frame(name=names(df),
                    db.name=make.sql.names(names(df)),
                    type=sapply(df,type),
                    label=sapply(df,label))
  names(df) <- map$db.name
  fullname <- paste(schema,name,sep=".")
  connect(...)
  con <- getOption("svyDBConnection")
  copy_to(con,df,
          name=fullname,
          temporary=FALSE,
          overwrite=overwrite,
          indexes=indexes
  )
  copy_to(con,map,
          name=paste(fullname,"map",sep="_"),
          temporary=FALSE,
          overwrite=overwrite,
          indexes=list("db.name")
  )
  t <- tbl(con,fullname)
  m <- tbl(con,paste(fullname,"map",sep="_"))
  #dbDisconnect(getOption("svyDBConnection"))
  list(data=t,map=m)
}

# raw applies to plain data.frames, so the attributes are not checked
make.sql.names <- function(ns,max.len=63,raw=FALSE,remove.group=TRUE){
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

