## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  data <- x$get()
  if (!is.null(i) & data == x) {
    message("getting cached data")
    return (i)
  }
  i <- solve(data, ...)
  x$setInv(i)
  i
}
