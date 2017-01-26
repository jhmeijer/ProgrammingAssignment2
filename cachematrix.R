# Coursera programming assignment 2.
# Jeroen Meijer

# Example of useage of the 2 functions:
# matr <- matrix(1:4,2,2)
# a <- makeCacheMatrix(matr)
# cacheSolve(a)
# cacheSolve(a)

## Define functions to set all values to initial values, 
## get the current matrix value,
## set the cached inverse of a matrix into cache memory,
## get the cached inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
    inv   <- NULL
    set <- function(y)
    {   x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getCachedInverse <- function() inv
    list(set = set, get = get, setInverse =setInverse, 
         getCachedInverse = getCachedInverse)
}


## Calculate the inverse of a matrix, or retrieve it from cache
## if it has been calculated previously. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getCachedInverse()
    if( !is.null(inv))
    {   message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve( data, ...)
    x$setInverse(inv)
    inv
}
