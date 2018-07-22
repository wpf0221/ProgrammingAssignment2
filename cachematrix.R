## Put comments here that give an overall description of what your
## functions do
## These two functions cache the inverse of a matrix, to avoid computing
## the inverse of a unchanged matrix repeatedly

## Write a short comment describing this function
## This function creates a "matrix" object, which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y){
    x<<-y
    x_inv<<-NULL
  }
  get <- function()x
  setinverse <- function(inverse)x_inv<<- inverse
  getinverse <- function()x_inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the "matrix" object created by
## the function makeCacheMatrix above. This function should retrieve
## the inverse from the cache, if the inverse was calculated 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
  }
