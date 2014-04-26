

## This function creates a special matrix that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        # i is the inverse matrix and m is the actual matrix
        i = NULL
        m <<- x
        
        ## Following functions are self explanatory.  
        # getInverse returns the matrix i -> cached value
        getInverse <- function() i 
        # setInverse sets the inverse matrix, ie i
        setInverse <- function(y)  i <<- y        
        # get returns the original matrix and set sets the matrix.
        get <- function() m
        # set resets the inverse matrix when the matrix changes
        set <- function(y) {
            if (! identical(m, y)) {
                m <<- y
                i <<- NULL
            }
        }
        # Returns the list of functions that operates on the object
        # Part of closure object. 
        list(getInverse = getInverse, 
             setInverse = setInverse,
             get = get, set = set )
        
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Do we have an inverse already cached. Inverse will be NULL 
    # if the matrix is changed or inverse is never calculated.
    if (is.null(x$getInverse())) {
        message("Set cache data")
        x$setInverse(solve(x$get()))
    }
    else {
        message("Getting from the cache")
    } 
    # Finally return the inverse.
    x$getInverse()
}


# Following are the test code. 
# Create a special matrix
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
# We have not called cacheSolve and hence the result will be NULL
a$getInverse()
# Following should print the message "Set cache data" since this is the
# first time an inverse is computed for this matrix
cacheSolve(a)
# Following should print same result
a$getInverse()
# Call cacheSolve once again.  We should see "Getting from the cache" message
# as an inverse is already computed and the matrix is not changed.
cacheSolve(a)

# Change the matrix.  This should invalidate inverse as well.
a$set(matrix(5:8,2))
a$getInverse()

# Following should show the message "Set cache data" as the matrix is changed.
cacheSolve(a)