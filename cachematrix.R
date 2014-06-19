## The functions in this file are useful to cache and retrieve inverse of a matrix. 
## Since computing inverse of a matrix is a time consuming task, it is preferred to
## store and retrieve it from cache.

##USAGE:
##1. call m<-makeCacheMatrix(x=matrix(1:4, nrow=2, ncol=2))
##2. call cacheSolve(m) would return the inverse of x by computation and updates cache
##3. call cacheSolve(m) would return the inverse of x from cache

## The function makeCacheMatrix makes a special matrix which caches the inverse of the
## matrix passed via setInverse(inMatrix)

## The input x must be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inMatrix) inverse <<- inMatrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function cacheSolve returns the inverse of the special matrix passed as argument,
## from the cache (if present), else calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inMatrix <- x$getinverse()        
        if(!is.null(inMatrix)) {
                cat("getting cached data", "\n")
                return(inMatrix)
        }
        cat("NOT getting cached data", "\n")
        data <- x$get()
        inMatrix <- solve(data, ...)
        x$setinverse(inMatrix)
        inMatrix
}

