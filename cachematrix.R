## These functions together return the inverse of a given matrix. It either calculates the inverse or returns the previously calculated (cached) inverse.

## This function sets the value of a given matrix, gets the value of a given matrix, sets the value of the inverse of the matrix and gets the value of the inverse. If the inverse has not previously been calculated, then this function will return a NULL value.

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


## This function either calls the cached matrix if the inverse has previously been calculated or it calculates the inverse of the given matrix if it has previously not been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
