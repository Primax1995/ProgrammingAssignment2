## The function cacheSolve inverts a matrix defined by the command makeCacheMatrix checking before if the 
## inverted Matrix is alread in the Cache. If not it goes trough the inversion
## function and stores the solution in the Cache.

##   - 'set' = set the value of the matrix
##   - 'get' = get the value of the matrix
##   - 'setInvert'= set the value of the inverse matrix
##   - 'getInvert' = get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvert <- function(invert) m <<- invert
    getInvert <- function() m
    list(set = set, get = get,
         setInvert = setInvert,
         getInvert = getInvert)
}


## this function checks if the inverted matrix is already in the cache and if not
## inverts the matrix and stores it in the cache

cacheSolve <- function(x, ...) {
    m <- x$getInvert()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvert(m)
    m
}
