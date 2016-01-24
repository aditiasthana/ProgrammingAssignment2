## The following two functions are used to compute and cache the inverse of a 
## matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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


## cacheSolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it returns the cached result  
## and skips the computation. If not, it computes the inverse and sets the 
## value of inverse via setInverse function.

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