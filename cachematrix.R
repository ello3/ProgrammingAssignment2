
## This function creates a special "matrix" obj that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
    x <<- y
    invx <<- NULL
    
  }
  get <- function() x
  setinvxerse <- function(invxerse) invx <<- invxerse
  getinvxerse <- function() invx
  list(set = set, 
       get = get,
       setmean = setmeat,
       getmean = getmean)

}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  invx <- x$getinvxerse()
  if(!is.null(invx)){
    message("getting cached data")
    return(invx)
  }
  m <- x$get()
  invx <- solve(m, ...)
  x$setinvxerse(invx)
  invx
  
}
