# These functions allow us to cache a matrix and its inverse so that we only have to calculate the inverse once.
# This could help to optimise our code if we were dealing with a large matrix or calling the function many times.

# The functions can be used as follows:
# m <- makeCacheMatrix(matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)) ## Puts the matrix in memory and gives us the functions we need.
# cacheSolve(m) ## Calculates the inverse, caches it, and returns it.
# cacheSolve(m) ## Returns the cached version of the inverse.

# This function returns the list of functions we can use to interact with the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Try to get the cached inverse of the matrix from the list passed in.
# If it isn't there, find it and cache it for next time. Notify when the cache is used so we can see it's working.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
