## Put comments here that give an overall description of what your
## functions do

##this function returns a list of functions which do the following 
##deeds:
##----------
#   - makeMatrix: sets a new matrix even if you did not input it 
#in the function initially
#   - getMatrix: recalls the matrix
#   - copyCachedInv: runs with the 'cacheSolve' function to cache 
#the inverse matrix
#   - getCache: returns the cached inverse matrix calculated using
#'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    make <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    copyCachedInv <- function(inverse) inv <<- inverse
    getCache <- function() inv
    
    funclst <- list(make = make, get = get,
         copyCachedInv = copyCachedInv,
         getCache = getCache)

    return(funclst)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getCache()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    originalMatrix <- x$get()
    inv <- solve(originalMatrix, ...)
    x$copyCachedInv(inv)
    return(inv)
}
