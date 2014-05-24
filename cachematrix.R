## Functions to define a matrix object with a cached inverse

## Defines a matrix object whose inverse, once computed
## will be cached in the object.
## has methods to get (returns the matrix)
## set (set the matrix)
## getInverse (returns the inverse matrix)
## and setInverse (set the inverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix){
        x <<- matrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(m) inverse <<-m
    getInverse <- function() inverse
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## if the matrix inverse is already cached, return it
## otherwise, calculate it, cache it, and return it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        return(inverse)
    }
    matrix = x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}