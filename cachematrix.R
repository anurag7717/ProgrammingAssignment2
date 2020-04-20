## This program creates a matrix using a function, computes its inverse,
## and computes it.

## This function is used to create and get both the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
     x <<- y
     i <<- NULL
   }
   get <- function() x
   setinv <- function(inv) i <<- inv
   getinv <- function() i
   list(set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)

}


## This function computes the inverse of the matrix and if it has already been
## calculated, cacheSolve retrieves the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
