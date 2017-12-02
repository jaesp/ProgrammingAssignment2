## Programming Assignment 2 - R Programming
## Data Science Specialization Track

## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly (there are also
## alternatives to matrix inversion that we will not discuss
## here). Your assignment is to write a pair of functions
## that cache the inverse of a matrix.

## makeCacheMatrix:
## To facilitate caching, we need to first create a special
## matrix. The input into this function is simply a variable of type matrix.


makeCacheMatrix <- function(x = matrix()) {
    # Following the same format as the assignment example
    # makeCacheMatrix will return a list containing
    # four functions that:
    # 1. set the matrix
    # 2. get the matrix
    # 3. set the inverse of the matrix
    # 4. get the inverse of the matrix
    # This list is used as input to cacheSolve()
    
    # Initially has no value
    # Changes when the user sets the value
    inv <- NULL
    
    # set function
    # Sets the matrix itself but not the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinv <- function() inv
    
    # Put together into a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)	
}

## cacheSolve:
## Once you create this matrix, we use the cacheSolve
## function to compute the inverse and cache the result

## If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' (x is the output of makeCacheMatrix)
    # Following the same format as the assignment example
    
    # Checks to see if inverse has been computed already
    inv <- x$getinv()
    
    # If it has...
    if(!is.null(inv)) {
        # Simply return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }
    
    # If it hasn't...
    # Get the matrix itself
    data <- x$get()
    
    # Find the inverse
    inv <- solve(data, ...)
    
    # Cache this result in the object
    x$setinv(inv)
    
    # Return this new result
    inv    
}