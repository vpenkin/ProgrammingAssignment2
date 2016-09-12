
## This function returns list which contains 4 functions

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    list(
        set = function(y) {
            x <<- y
            inverseMatrix <<- NULL
            }, 
        get = function() x,
        setinverse = function(invM) inverseMatrix <<- invM,
        getinverse = function() inverseMatrix
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    
    # Return cached value, if present
    if (!is.null(m)){
        return(m)
    }
    
    # Calculate the inverse matrix
    m <- solve(x$get(), ...)
    x$setinverse(m)
    
    return(m)
}