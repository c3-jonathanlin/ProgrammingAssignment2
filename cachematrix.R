## Functions that allow caching of the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
