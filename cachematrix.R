## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a "special matrix" internally storing the matrix provided
# as argument together with its inverse.
#
# The returned list provides allows to have access to this "special matrix" through the its functions content:
# • get: returns the internal matrix
# • set: sets internal matrix
# • getInverse: sets internal matrix
# • setInverse: sets internal matrix
#
# Defaults value for the m argument is a1 x 1 unitialised matrix
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve takes as argument a "special matrix" as created by the 
# makeCacheMatrix function above.
# It returns the inverse of its matrix argument. This inverse is computed only
# when required then cached in the "special matrix" to avoid recomputation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
