## Functions to cache the inverse of a matrix

## makeCacheMatrix function will create a special matrix object, it can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialisation of the inverse matrix
        inv_mat <- NULL
        
        # We got the setter function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x # for getter 
        # We set the special inverse matrix
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## cacheSolve compute the inverse of the special matrix that we create with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInverse()
        # If the inverse matrix is not null we return it & we send a message that we cached the data we want
        if (!is.null(inv_mat)) {
                message("Cached Data !")
                return(inv_mat)
        }
        
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
        inv_mat # return the inverse of matrix
}
