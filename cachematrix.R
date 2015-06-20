## The following functions can be used to create a sort of caching object
## that can be used to hold both a matrix and its inverse.  Additionally,
## it provides a method that can intelligenly leverage the cache to ensure
## that the inverse is only calculated once.  As of yet,
## there has been no work to ensure that the matrix is actually invertible.

## This function creates a list containing a set of functions to
## get/set a matrix and its inverse.
## An example usage is: cached = makeCacheMatrix(matrix(c(1, 2, 7, 8), 2, 2, byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    set = function(y){
        x <<- y
        inv = NULL
    }
    
    get = function() x
    setInverse = function(inverse) inv <<- inverse
    getInverse = function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function can be used to retrieve the inverse of a matrix
## that has been stored in an object that was created by the 
## 'makeCacheMatrix' function above.  If the inverse has already
## been calcuated, then the previous value will be used instead of
## computing the inverse again.
## An example usage is (based on example from above): cacheSolve(cached)
## Subsequent executions of 'cacheSolve' with the same object will automatically retrieve the cached value.

cacheSolve <- function(x, ...) {
    inv = x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    
    data = x$get()
    inv = solve(data, ...)
    x$setInverse(inv)
    
    inv
}
