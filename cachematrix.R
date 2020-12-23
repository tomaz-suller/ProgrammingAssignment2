## Put comments here that give an overall description of what your
## functions do

## Optionally receives a matrix 'x'; returns a list of functions to interface with such matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Checks if the inverse of 'x' is cached, and if not calculates and caches it. Returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ... {
        if( is.null(x$getInverse()) ){
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                return(inv)
        }
        x$getInverse()
}
