## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        x_inv = solve(x)
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(x_inv) i <<- x_inv
        getInverse <- function() i
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getInverse()
        if(!is.null(x_inv)) {
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data)
        x$setInverse(x_inv)
        return(x_inv)
}
