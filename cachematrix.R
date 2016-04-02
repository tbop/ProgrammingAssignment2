## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Here is a pair of utility functions that cache the inverse of a matrix.

## creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setinvert <- function(inv) invert <<- inv
    getinvert <- function() invert
    list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invert <- x$getinvert()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinvert(invert)
    invert
}

## Example to test:
## x <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
## m <- makeCacheMatrix(x)
## inv <- cacheSolve(m)
## inv should display
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1

