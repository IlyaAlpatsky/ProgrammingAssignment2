## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) r <<- solve
  getsolve <- function() r
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getsolve()
  if(!is.null(r)) {
    message("getting cached solution, matrix was not changed")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setsolve(r)
  r
}
