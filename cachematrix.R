## This function takes a matrix as an input and inverts the matrix. Other functions within this function
## are used to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  print(getMatrix)
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  print(getInverse)
  ulist(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is used to retrieve the inverse of a matrix if already cached or compute the inverse if not available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
