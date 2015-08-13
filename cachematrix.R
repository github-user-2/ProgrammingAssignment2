## This file contains functions for a "cache matrix", which is a special "matrix"
## object that can cache the inverse of a matrix to avoid recalculation.
## The two methods defined in this file are:
##   makeCacheMatrix -- creates a "cache matrix" object
##   cacheSolve -- returns the inverse of a "cache matrix" object, using cached
##                 results if available.

##
## makeCacheMatrix(x)
##
## This function creates a "cache matrix" object, which is a closure that stores a
## matrix and its cached inverse (if computed).
##
## Parameters:
##   x -- The initial matrix to wrap in the cache matrix.  If no value is given
##        then an empty matrix is used.
##
## Returns:
## A list of functions for interacting with the cache matrix.  These functions are:
##   set(y) -- Sets the value of the matrix.  Clears any cached inverse.
##   get() -- Returns the matrix.
##   setinverse(inverse) -- Sets the cached inverse value of the matrix.
##                          This method should NOT be called by clients, but only
##                          by the cacheSolve function.
##   getinverse() -- Returns the cached inverse value of the matrix.  While clients
##                   can call this method, it is better to call the cacheSolve method
##                   so that the inverse will be computed if not already cached.
##
makeCacheMatrix <- function(x = matrix()) {
    # Initially there is no value for the inverse
    inv <- NULL

    # Define the set of functions for interacting with this object
    set <- function(y) {
        x <<- y
        # Clear the cached inverse value, if any
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    # Return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## cacheSolve
##
## Returns the inverse of the "cache matrix".  If the inverse has not been cached
## already then the value is computed, cached, and returned.
##
## Parameters:
##   x -- The "cache matrix" object to retrieve the inverse for
##   ... -- Parameters to pass to the "solve" function if the inverse has not already
##          been cached.
##
## Returns:
## The inverse of the matrix stored in the "cache matrix".
##
cacheSolve <- function(x, ...) {
    # Check if a cached value for the inverse exists
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # The inverse has not been cached, so retrieve the matrix from the
    # cache matrix, compute the inverse, and store it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)

    # Return the computed inverse
    inv
}
