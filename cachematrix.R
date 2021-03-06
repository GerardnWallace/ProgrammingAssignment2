## makeCacheMatrix and cacheSolve are a pair of functions that allow a matrix 
## inverse to be calcualted and cached/retreived as needed to reduce the 
## compulated time associate with repeated matrix inversion

## makeCacheMatrix defines the functions required to calculate a matrix inverse
## used by cacheSolve.  makeCacheMatrix defines the parent environment for the 
## functions which will be used by cacheSolve.
##
## Arguements: A matrix
## Returns: A list of functions

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve computes and caches the inverse of x the first time x is passed to 
## cacheSolve.  On subsequent calls to cacheSolve(x) the cached value is 
## retreived.
##
## Arguements: x, A list of functions  
## Returns: The inverse of X

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}



