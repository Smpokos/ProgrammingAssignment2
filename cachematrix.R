## Pair of functions makeCacheMatrix & cacheSolve that evaluate and
## cache the inverse matrix of the input matrix and give the inverse
## if it has been evaluated before

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL 
        setMatrix <- function(y) {
                x <<- y
                M <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(mat) M <<- mat 
        getInverse <- function() M
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.  If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        M <- x$getInverse()
        if(!is.null(M)){
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x' had been calculated
                return(M)
        }
        data <- x$getMatrix()
        M <- solve(data, ...)
        x$setInverse(M)
        ## Return a matrix that is the inverse of 'x'
        M
        
}
