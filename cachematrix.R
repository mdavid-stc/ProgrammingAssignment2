## Define an object that can perform matrix inversion more efficiently.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # There are two local variables:
    #   x - a matrix
    #   inv - the inverse of x
    inv <- NULL

    # Set the main matrix (which causes the inverse to be reset)
    set <- function(y) {
        x <<- y
        # Since x changed, clear the inverse.
        inv <<- NULL
    }

    # Get the original matrix
    get <- function() x

    # Set the inverse matrix
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse matrix
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
##   makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
##   then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    # Check if inverse has already been calculated and cached
    if (!is.null(inv)) {
        # If so, take the easy way out.
        message("Using cached version")
        return(inv)
    }
    # If there is no cached version, get the original and invert it.
    data <- x$get()
    # Note: assumes that the matrix supplied is always invertible
    inv <- solve(data, ...)
    # Cache the inverted version for next time.
    x$setinverse(inv)
    inv
}
