## Exposes the ability to create a matrix which can cache 
## the value of its inverse

## This function takes a matrix and returns an object which 
## enables access to that matrix as well as the ability to 
## set, store and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes a cache matrix as returned by 
## makeCacheMatrix and returns the inverse of the matrix.
## The inverse is stored within the cache matrix and the 
## stored value is returned on subsequent calls, so long 
## as the input variable does not change.

cacheSolve <- function(x, ...) {
    ## Check to see if we've already cached the inverse,
    ## and just return that value if we have.
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i) ## Store the inverse back in 'x'
    i ## return the inverse
}
