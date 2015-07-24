## These two functions, 'makeCacheMatrix' and 'cacheSolve' work together
## to make it save and return the inverse of a matrix. This functionality
## is important because it obviates the need to continually recompute an
## inverse for a matrix that is being reused in a set of computations

## The function 'makeCacheMatrix' creates a list of functions which 
## manage the states of the matrix of interest. This list is actually ## a special object that holds the inverse of the current matrix within ## the environment containing the function. This ia a form of cacheing.
 
 makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function 'cacheSolve' can check to see if a cached inverse 
## exists and if not, can perform the process. It can also return the ## inverse. As such, this is the working function, as it calls the 
## functions contained within 'makeCacheMatrix' as needed.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x' 
		 inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}