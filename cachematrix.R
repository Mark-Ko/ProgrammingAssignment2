## The function makeCacheMatrix creates a list object that stores the
## matrix inverse calculated in the function cacheSolve.

## The two functions work together to solve for the inverse 
## only if the input matrix has changed.


## makeCacheMatrix creates a matrix object that is a list containing
## functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  # x is the input matrix
        
        m <- NULL                            # inverse, reset everytime
                                             # function called
        
        
        set <- function (y) {                # save input and reset
                x <<- y                      # inverse to NULL
                m <<- NULL
        }
        
        get <- function() x                  # return value of original
                                             # matrix x
        
        setinverse <- function(solve) m <<- solve  # called by cacheInverse,
                                                   # stores value of inverse
                                                   # on first call                                                     
        
        getinverse <- function() m           # returns cached inverse to 
                                             # cacheInverse on subsequent
                                             # calls
        
        ## Return the list of functions
        list(set=set,get=get, setinverse =setinverse, 
             getinverse=getinverse)
        
}


## cacheSolve returns the inverse of the matrix 'x'
##     created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        m <x$getinverse()         # get the value of inverse
        
        # If 'x' hasn't changed return cached inverse of 'x'
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        # If 'x' has changed calculate the inverse of 'x'
        data <- x$get()           # get the new data
        
        m <- solve(data, ...)     # calculate inverse of new matrix
        
        x$setinverse(m)           # store new inverse in setInverse
        
        # Return a matrix that is the inverse of 'x'
        m
        
}
