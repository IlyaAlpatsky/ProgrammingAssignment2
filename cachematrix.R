## The file contains 2 functions MakeCacheMatrix and cacheSolve
## functions create an object (MakeCacheMatrix) and inverse matrix 
## or load inversed matrix from cache if it has been already calculated

## The function create an object with methods to set value and get value of
## original matrix and for setinverse and getinverse methods
## f.e. init object:
## b<-makeCacheMatrix(mat)  
## inverse matrix 1st, real inversion and write result to cache:
## b_inv <- cacheSolve(b) 
## read from cache because already inversed matrix has not been changed yet
## b_inv <- cacheSolve(b)
## set new matrix, chached inversed matrix will be unset to NULL:
## b$set(mat_new)
## b_inv <- cacheSolve(b)
## Real calculation, because inversed matrix has bee just unset



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


## The function iverse or load inversed matrix from cache
## the function work only with object created by MakeCacheMatrix
##

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
