## The following code demonstrates how to cache potentially time-consuming computations in R.
## This specific example shows how to do this for computing the inverse of a matrix.
## If the contents of a matrix are not changing, it may make sense to cache the value
## of the inverse so that when we need it again, it can be looked up in the cache 
## rather than recomputed

## makeCacheMatrix creates a special "matrix", which is really a list 
## of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. setinverse to set the value of the inverse
## 4. getInverse to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(inputMatrix) {
        x <<- inputMatrix
        # Invalidate inverse (cache result) since we are changing the matrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inputInverse) inverse <<- inputInverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it  get the inverse
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via 
## the  setinverse  function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    ## Compute the inverse of the matrix. Assume matrix is invertible
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse    
}
