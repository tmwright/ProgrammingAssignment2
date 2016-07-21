## cacheMatrix.R -- Two functions that leverage R's lexical scoping to
## cache the inverse of a matrix.
## Tod Wright 21 July 2016


## makeCacheMatrix returns a list of setters and getters that address
## the stored matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the matrix inverse, using the cached value if
## it is available.  Otherwise it calculates and caches the inverse
## before returning it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
