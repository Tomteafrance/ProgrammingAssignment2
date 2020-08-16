## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x # for getter 
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInverse()
        if (!is.null(inv_mat)) {
                message("Cached Data !")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setInverse(inv)
        inv_mat
}
