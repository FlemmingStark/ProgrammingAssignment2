##this code creates two functions to enable the user to have a spacial matrix object
##that can contain the inverse value and another function to compute the inverse
##or return the one already cached in previous computes with the same values in the matrix

makeCacheMatrix <- function(x = matrix()) {
##This function create an special matrix taking a matrix objext as input
    inverse <- NULL
    set <- function(y) {
        x <<- y
        ##this set the inverse to NULL in every change to make sure the cached value
        ##is not from a different value than the current one
        inverse <<- NULL
    }
    get <- function() x
    ##this make it able to store the value and the inverse in the same object
    setinverse <- function(value) inverse <<- value
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
##This function receive a matrix object and return its inverse

    ##first checking if it was not computed before and the matrix have not changed
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        #and returning the cached value if it was
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse

}
