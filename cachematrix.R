## Calculate, cache and retrieve the inverse of a matrix
### The script contains two main functions,
### makeCacheMatrix and cacheSolve

## MakeCacheMatrix takes a matrix argument and
## returns a list of functions to retrieve
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Create a list object whose members comprise of four functions
    ## to set and get the matrix 'x',
    ## and to set and get the inverse of the matrix 'x'.
    
    # Initialise inv and define 'set matrix' function
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Define 'get matrix' function
    # and setter + getter for the inverse.
    # return a list of all setters and getters.
    get <- function() x
    setinverse <- function(matrix_inverse) inv <<- matrix_inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is similar to the solve function,
## but takes a makeCacheMatrix as argument
## and returns the cached inverse of the matrix, if available.
## Otherwise it calculates, caches and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check if the inverse of x is cached and return it if available.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Use solve function to find the inverse of the matrix.
    # Then caches the result and returns it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}