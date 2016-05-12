

## Write a short comment describing this function

# Function description makeCacheMatrix
# This function returns a list that contains 4 functions
# 1) Set the matrix
# 2) Get the matrix
# 3) Set the inverse of a matrix
# 4) Get the inverse of matrix
# Furthermore the inverse is set equal to "null"in case the function is called directly or
# the setMatrix function is called.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverseValue) inverse <<- inverseValue
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function

# Function description cacheSolve
# This function returns the inverse of a matrix that has served as input for the makeCacheMatrix
# It first checks if the inverse is allready calculated:
#   If so: report the saved inverse
#   If not: calculate the inverse of the matrix and set the inverse. Then return the inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

