## Put comments here that give an overall description of what your
## functions do
             ##Below are a pair of functions that are used to create a special object that 
             ##stores a matrix and caches its inverse.

## Write a short comment describing this function
             ##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     Minv <- NULL
  set <- function(y){
  x <<- y
 Minv<<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) Minv <<- inverse
  getInverse <- function() Minv
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## Write a short comment describing this function
                 ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
                 ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
       
       ## Return a matrix that is the inverse of 'x'
         Minv <- x$getInverse()
  if(!is.null(Minv)){
  message("getting cached data")
  return(Minv)
  }
  M <- x$get()
  Minv <- solve(M,...)
  x$setInverse(Minv)
  Minv
}
