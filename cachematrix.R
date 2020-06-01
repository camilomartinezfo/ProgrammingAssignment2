## Put comments here that give an overall description of what your
## functions do

## Functions that cache the inverse of a matrix.


## The function makeCacheMatrix creates a list of functions, where are storaged the matriz,
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y){
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inversa <<- solve
        getinverse <- function() inversa
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve generates the inverse of the matriz of the object created with the previous
## function, but if there isn't an result, then this function calcules the inverse.

cacheSolve <- function(x, ...) {
        inversa <- x$getinverse()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        matriz <- x$get()
        inversa <- solve(matriz)
        x$setinverse(inversa)
        inversa
}



