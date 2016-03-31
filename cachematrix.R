## META INFO
## ==========
## Author: Asad Khan
## Coursera: R Programming - Lexical Scoping
## Date Submit: 2016-03-30
##
## ASSUMPTIONS
## =============
## x is assumed to be a valid invertible matrix
## makeCacheMatrix ==> takes a matrix as input and returns a function that will be used
## by cacheSolve
## 
## cacheSolve ==> uses the return of the makeCacheMatrix and validates to see if the Matrix is already Cached
## if yes, it will return cached version, otherwise will calculate a new matrix

## TESTING
##
## > x <- matrix(rnorm(400),nrow=20,ncol=20) 
##
## 		MUST BE A SQUARED MATRIX
##
## > r <- makeCacheMatrix(x)
## > cacheSolve(r)
## will result in a inverted matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


cacheSolve <- function(x, ...) {
       
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}