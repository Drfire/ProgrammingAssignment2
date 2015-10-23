## These programs create a cache for a matrix, and then invert the matrix. There are two functions:
## first makeCacheMatrix to create the cache and compute the inverse. Second, cacheSolve, to access 
## inverted matrix out of the cache. 


## makeCacheMatrix creates a cache for an inverted matrix that is input into the function cacheSolve

makeCacheMatrix <- function (x = matrix()) {
  m <- NULL             ## initializes null matrix
  set <- function(y) {  ##creates the cache
    x <<- y       
    m <<- NULL
  }
  get <- function() x    
  setinverse <- function(solve) m <<- solve   ## uses solve function to get inverse
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Calculates the inverse of a matrix that is cached in the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  m <- x$getinverse()     ## gets inverse
  if(!is.null(m)) {
    message("getting cached data - inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}