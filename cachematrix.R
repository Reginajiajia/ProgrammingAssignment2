## Create an invertible matrix and cache the inverse of the matrix.

## The 1st function creates a "Matrix". which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        #Initialize the inverse value
        xInverse <- NULL  
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) xInverse <<- solve
        getInverse <- function() xInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The 2nd function solve the inverse of the matrix created with the 1st function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInverse <- x$getInverse()
        if(!is.null(xInverse)) {
                message("getting cached data")
                return(xInverse)
        }
        data <- x$get()
        xInverse <- solve(data, ...)
        x$setInverse(xInverse)
        xInverse
}
