## This pair of functions creates the ability to cache the inverse of a matrix 
## and return the cached version instead of calculating it directly 
## on subsequent calls.  

## The first function creates a list a four functions to capture the initial 
## matrix and it's inverse.  It returns the function definitions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function takes the list object from and either computes and returns
## the inverse matrix if it has not been previously created.  If it already exists,
## it returns the cached version.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
