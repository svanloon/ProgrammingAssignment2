## Provide a caching mechanism for a matrix's inverse
## to cut down computation costs

## makeCacheMatrix decorates a matrix by adding a inverse property
## get - returns the original matrix
## set - updates the matrix and invalidates the inverse
## setmean - set the value of the inverse property
## getmean - get the value of the inverse property

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' is a utility method to retrieve the
## inverse property from the wrapped matrix.
## If the inverse property is not set, it will
## solve the inverse and set the property.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}