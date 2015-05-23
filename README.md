makeCacheMatrix <- function(x = matrix()) {
  m<-NULL ## set value of m to Null
  set<-function(y){ ## set value of matrix
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


cacheSolve <- function(x=matrix(), ...) { ## compare matrix to previous 
    m<-x$getmatrix() ## if an inverse is calculated already this function will get it for you
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m) ## use this to set te matrix function on whatever you are inputting matrix to cache
    m ## this will return inverse
}
