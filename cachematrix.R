## Taking the mean can be a length process through the utlization of 
## multiple loops, we can, however, cache the mean of so when we 
## need it we can look at it again.

## Makes martix which can be inversed by cache

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Creates inverse of matrix made previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    
    return(inv)
  }
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}
