## The two functions below are used to create a special object 
## that stores a matrix and caches its inverse.

## The initial function, makeCacheMatrix, creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The second function, cacheSolve, computes the inverse of the special "matrix"
## returned by the first function.  cachesolve retreives the inverse from the 
## cache if it has already been computed.

cacheSolve <- function(x, ...) {
    i <-x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}
