## Functions makeCacheMatrix and cacheSolve are used to calculate
## inverse of matrix with ability of caching inversion results.

## Function makeCacheMatrix creates a caching object which is used
## to store a cached inverse of a matrix and functions to manipulate the cache.
## If matrix is changed then cache is reset.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(newMatrix) {
        x <- newMatrix
        cache <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) cache <<- solve
    getSolve <- function() cache
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Function cacheSolve calculates inverse of special cache matrix returned by makeCacheMatrix. 
## It uses cached value from the cache matrix if it's already calculated.

cacheSolve <- function(x, ...) {
    cachedValue <- x$getSolve()
    if (!is.null(cachedValue)) {
        message("Cached value")
        return(cachedValue)
    }
    data <- x$get()
    matrixInversion <- solve(data, ...)
    x$setSolve(matrixInversion)
    matrixInversion
}
