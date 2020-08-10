## Two functions below are used to create a special object 
## that stores a matrix and cache's its inverse.

## This function creates matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) i<<-inverse
  getInverse<-function() i
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function computes the inverse of the matrix returned by function above and retrieves the already calculated inverse from the cache.
cacheSolve <- function(x, ...) {
  i<-x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
