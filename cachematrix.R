## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions that will get and set the
## value of a matrix and the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
	  set <- function(y) {
		    x <<- y
		    m <<-NULL
	  }
	  get <- function() x
	  setsolve <- function(solve) m <<- solve
	  getsolve <- function() m
	  list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)

}


## This function searches for a cached version of the inverse of
## a matrix. If none is found, it will use solve() to calculate the
## inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getsolve()
	  if(!is.null(m)) {
		    message("getting cached data")
		    return(m)
        }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setsolve(m)
	  m

}
