## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" creates a special "matrix" containing a function to do below:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y){
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<-solve
      getsolve <- function() s
      list(set = set, get=get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## "cacheSolve" calculates the inverse of the special "matrix" created with "makeCacheMatrix". 
## check to see if the inverse has already been calculated. 
      ## If so, it gets the inverse from the cache and skips the computation. 
      ## Otherwise, it calculates the inverse and sets inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if (!is.null(s)) {
            message("getting cache data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
