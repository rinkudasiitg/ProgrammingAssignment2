## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  
  ## set the value of the matrix
  set  <- function(y){
    x <<- y
    inv <<- NULL 
  }
  
  ## get the value of the matrix
  get  <- function() x
  
  ## set the value of the inverse
  set_inverse  <- function(inverse) inv  <<- inverse
  get_inverse  <- function() inv
  list(set= set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv  <- x$get_inverse()
  ## First checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
  ## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the set_inverse function
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data  <- x$get()
  inv  <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
