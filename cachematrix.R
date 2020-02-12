## The purpose of this code is to calculate the inverse of the square matrix and
## store the result of the calculated matrix so that we can read in the result 
## at any time without recalculating

## makeCacheMatrix function creates special vector which includes 4 functions.
## set function sets the matrix, get function retrieves the matrix
## setInverse function stores the inverse of the matrix
## getInverse function retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(cal_inv) inv <<- cal_inv
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function is used to calculate the inverse of the square
## matrix. If the inverse of the corresponding square matrix has been 
## calculated already, then get the result from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
