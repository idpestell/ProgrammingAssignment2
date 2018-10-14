## function 'makeMatrixCache' is an object containing a matrix and its inverse
## Which has the following functions
##  set - which sets the matrix and clears the inverse
##  get - which gets the matrix
##  getinverse - which gets the inverse of the matrix or NULL if no inverse stored
##  setinverse - which sets the value of the inverse matrix

## function which conatins a matrix and a cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
    # The inverse of the matrix cache
    inv  <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get matrix
    get <- function() { x }
    
    # Function to set the inv
    setinverse <- function(theinv) {
        inv <<- theinv
    }
    
    # Function to get the inverse
    getinverse <- function() { inv }
    
    list( 
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )

}


## Returns cached version of the inverse of x or calculates and caches inverse if not
## already calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Get current value of
    inv <- x$getinverse()
    
    # Test to see if needs to be calculated
    if( is.null(inv) ) {
        # Calculate
        m <- x$get()
        inv <- solve(m)
        # set back into x
        x$setinverse(inv)
    }
    
    return(inv)
}