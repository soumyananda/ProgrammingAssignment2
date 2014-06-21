## This library implements inverse of a square matrix, and makes use of caching
## to avoid repeated redundant computation of the inverse.

## This function takes a matrix as input parameter, and returns a list containing 4 functions
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of a matrix that has already been processed by the above function.
## It first checks if the inverse has already been calculated, and if yes, returns the value from the cache.
## Otherwise, it computes the inverse and saves it in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
