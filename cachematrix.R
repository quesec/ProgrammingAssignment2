## The following functions can be used to create an object which caches the 
## original matrix and its inverted form so calculation of the inverted form
## is only required once.
##  - makeCacheMatrix - creates the object
##  - cacheSolve - calculates and returns the inverted matrix on first call, 
##    or returns the cached version in future calls

## Creates an object which contains:
# - A cached inverted matrix once calculated (or NULL if not calculated)
# - A list of functions to:
#      - get the original matrix (get)
#      - set a new matrix (set)
#      - get the cached inverted matrix (getInvert)
#      - cache the inverted matrix (setInvert) 

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInvert <- function(solve) invert <<- solve
        getInvert <- function() invert
        list(set = set, 
             get = get, 
             setInvert = setInvert, 
             getInvert = getInvert)
}


## Checks if the inverted matrix calulation has been performed on x
##   - if yes - return the cached matrix
##   - if no - calculate the inverted matrix, cache the value, and 
##     return the calculated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getInvert()
        if(!is.null(invert)) {
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setInvert(invert)
        invert
}
