## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function to creates a special "matrix" object that can cache its inverse using an interval variable
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  imatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(imatrix = imatrix, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function helpsto  compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## First time is called calculates inverse and stores it in an internal variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
