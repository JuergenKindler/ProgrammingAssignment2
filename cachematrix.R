## makeCacheMatrix creates an environment for a matrix and its inverse
## and returns a list of functions bound to that environment that allow
## accessing the original matrix and the inverted one (getsolve, setsolve)
## 'x' is the original matrix to be cached
##
## Returns a list of accessor functions:
## set - set a new matrix value
## get - get the original matrix value
## setsolve - update the inverted matrix (caching!)
## getsolve - get the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set allows changing the matrix to be solved
    ## 'y' is the new matrix that replaces the old one
    ## Calling set clears the previously calculated inverted matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get allows reading the currently assigned original matrix
    get <- function() x
    
    ## setsolve allows to set the new inversion of x
    ## 'solve' is supposed to be set to solve(x)
    setsolve <- function(solve) m <<- solve
    
    # getsolve returns the cached inversion of x
    getsolve <- function() m
    
    # The function result is simply a list of the inner functions
    # inside makeCacheMatrix which are bound to the same names
    # ( name <- inner function )
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve returns the calculated inverse
## 'x' is a environment + list created by makeCacheMatrix
##
## Returns the inverted matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    
    # in case the inverted function was aready calculated, return it
    if(!is.null(m)) {
        message("Getting cached inverse matrix")
        return(m)
    }
    
    # Otherwise get the original matrix ...
    data <- x$get()
    
    # ... solve it ...
    m <- solve(data, ...)
    # ... cache the solution.
    x$setsolve(m)
    m
}
