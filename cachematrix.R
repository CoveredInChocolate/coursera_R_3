## makeCacheMatrix takes a matrix as input and returns a list.
## The list contains four functions:
##      set          - Stores the matrix
##      get          - Retrieves the matrix
##      setInverted  - Stores the inverted matrix
##      getInverted  - Retrieves the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverted <- function(solve) m <<- solve
    getInverted <- function() m
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)    
}


## If it cannot find anything in x$getInverted, the function
## stores the matrix in the variable data. Calculates the
## inverse and stores it in the variable m and finally stores
## the inverted matrix with x$setInverted(m)

## If the function finds something in x$getInverted(), we have
## already calculated the inverse and this is returned along
## with the message "getting chaced data".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverted(m)
    m
}
