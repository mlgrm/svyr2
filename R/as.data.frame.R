flatten <- function(x) UseMethod("flatten",x)
flatten.svq <- list
flatten.svg <- function(x)do.call(c,lapply(x,flatten))
flatten.svy <- flatten.svg
flatten.svr <- list

as.data.frame <- function(x,...) UseMethod("as.data.frame",x)

as.data.frame.svy <- function(s,simplify=TRUE,odk.names=TRUE,...){
  node <- lapply(s,attr,"node")
  # find protected matrices and flatten them
  # s <- lapply(s,as.data.frame)
  s <- mapply(function(e,a){
    # browser(expr=(any(sapply(e,length)==0)))
    attr(e,"node") <- a
    as.data.frame(e)
  }, s, node, SIMPLIFY = FALSE)
  s <- s[!sapply(s,is.null)]
  if(length(s)==0) return(NULL)
  browser(expr=is(tryCatch(do.call(cbind,s),error=identity),"error"))
  df <- do.call(cbind,s)
  if(odk.names)
    names(df) <- sapply(df,function(c){
      n <- name(c)
      if(length(group(c))>0) n <- c(group(c),n)
      if(type(c)=="select all that apply") n <- c(n,selected(c))
      n <- paste(n,collapse = "/")
      n
    })
  df
}

as.data.frame.svq <- function(x,...){
  if(type(x)=="note") return()
  if(is.matrix(x)){
    df <- as.data.frame.matrix(x)
    df <- as.data.frame(mapply(function(c,n){
      attr(c,"node") <- attr(x,"node")
      attr(c,"selected") <- n
      attr(c,"group") <- attr(x,"group")
      c
    },df,colnames(x),SIMPLIFY = F))
  } else {
    class(x) <- class(x)[class(x)!='svq']
    df <- data.frame(x)
    browser(expr=(length(df)==0))
    names(df) <- attr(x,"node")$name
    df
  }
}


as.data.frame.svg <- as.data.frame.svy

as.data.frame.svr <- function(r, simplify=FALSE, ...){
  # if simplify, put all responses in the group for each 
  # respondent in the same row.
  if(simplify){ # this might not be working
    r <- lapply(r,as.data.frame,simplify=simplify)
    # need to know how many columns we'll need
    len <- max(sapply(r,nrow)) 
    do.call(rbind,lapply(1:length(r),function(n){ # for every respondent household
      s <- r[[n]]
      browser()
      l <- lapply(1:ncol(s),function(qn){ # for every question in the group
        col <- s[[qn]]
        
        # make a vector of length len
        col <- c(col,rep(NA,len-length(col)))
        
        # transpose the column into a one-row data.frame
        col <- as.data.frame(t(col))
        
        # name the columns with the name of the question
        # and the respondent index
        names(col) <- paste(names(s)[qn],1:len,sep='/') 
      })
      
      # glue all the respondent dfs together into one long single row df
      s <- do.call(cbind,l)
    })) # and rbind those
  } else { # else just index them and rbind all the svys into one
    ind <- 1:length(r)
    attr(ind,"node") <- attr(r,"node")
    
    # the rbound dfs are now the data attribute of the svr
    attr(ind,"data") <- do.call(rbind,lapply(1:length(r), function(i){
      df <- as.data.frame(r[[i]])
      df <- cbind(ind=rep(i,nrow(df)),df)
    }))
    data.frame(ind)
  }
}
