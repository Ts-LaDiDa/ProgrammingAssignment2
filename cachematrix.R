## modified by: YT @ 20200201
##R version: 4.0.3; OS: darwin17.0

## first function establish the access channel to matrix, 
##and a holder for value "overhead management"
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  ## init end
  
  set <- function(m){
    x <<- m
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(invers) inver <<- invers
  getinver <- function() inver
  ## setter & getter end
  
  ##result output
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## the functionality block that actually do the calculation
## produce result and issue result caching
cacheSolve <- function(x, ...) {
  ## access
  inver <- x$getinver()
  if(!is.null(inver)){
    message("Retrieving cached result")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat,...)
  x$setinver(inver)
  ##result
  inver
}
