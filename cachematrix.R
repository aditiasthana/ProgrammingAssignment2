## This function creates a special vector that caches the calculation of its inverse
## It does this by creating the object as a list with `set` and `get` functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## initially the value of inverse is NULL
    
    ## set function assigns the new argument as the stored value and
    ##  reset the value of inverse to NULL
    set <- function(y) { 
        x <<- y
        inv<<- NULL
    }
    
    ## get function fetches the stored value of the object
    get <- function() x
    
    ## setInverse function sets the inverse value
    setInverse <- function(inverse) inv <<- inverse
    
    ## getInverse function gets the inverse value
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## This function will calculate the inverse of the square matrix and caching
## the result in the makeCacheMatrix object.
## If the inverse has already been computed,the cached result will be returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {  ## checking if inverse has already been computed
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) ## computing inverse
    x$setInverse(inv)  ##  setting inverse
    inv
}
