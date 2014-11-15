# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it
# repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions
# that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# It is in the form of a list of functions acting as proxies to the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        #message("getting cached data") #uncomment for debug, comment for performance
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
