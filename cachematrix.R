## This is part of Coursera course "R Programming" - Programming assignment 2
## This functions will cache the inverse of a matrix to avoid to calculate
## it repeatedly if it has already been calculated


## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse matrix to NULL until we have set and calculate
        ## for the first time the inverse of a matrix
        inverse <- NULL
        
        ## Function to set a new value for the matrix to calculate its inverse,
        ## so the inverse matrix cached in 'inverse' has to be set to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Funtion to get the matrix which its inverse will be calculated
        get <- function() x
        
        ## Function to set the inverse of a matrix, which is called then by
        ## 'CacheSolve'
        setinverse <- function(inv) inverse <<- inv
        
        ## Function which returns the inverse of a matrix
        getinverse <- function() inverse
        
        ## Return value of the 'makeCacheMatrix' function -> list of functions 
        ## to set and get the original matrix and its inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Get the inverse of the matrix defined in 'x', calling 'getinverse'
        inverse <- x$getinverse()
        
        ## If the result is not null (if it is already stored it with 
        ## 'setinverse' function), returns the inverse of the matrix, printing
        ## the message
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        
        ## If the result is null (because the inverse has not been cached), then
        ## it gets the matrix and calculates its inverse 
        data <- x$get()
        inverse <- solve(data, ...)
        
        ## Set the inverse in 'inverse' so it is cached for next time
        x$setinverse(inverse)
        
        ## Returns the cache inverse matrix
        inverse
}
