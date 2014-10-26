## Reilly, Donald J.      25 October, 2014
## DReil@Allstate.Com

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(Johnny = matrix()) {
        m <- NULL
        set <- function(y) {
                Johnny <<- y
                m <<- NULL
        }
        get <- function() Johnny
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(Johnny, ...) {
        
        m <- Johnny$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- Johnny$get()
        m <- solve(data, ...)
        Johnny$setinverse(m)
        m
}
