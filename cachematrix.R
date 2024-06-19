## These functions are being used to set, get, calculate, and cache data. This 
## is done in order to save computational power, and reduce overall load on the 
## machine. The first function sets and gets the value of the matrix, and the
## second function gets the value of the inverse from the cache (if it has 
## previously been calculated) or it will calculate and cache the data if not. 

## This function creates a "special" matrix, that we can use later on in the 
## second function. In this function, we set and get the value of the matrix, and
## then we set and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<- y
        inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This second function is used to either calculate and cache the inverse of the 
## matrix, or get the inverse of the matrix from the cache, if it has already 
## been calculated.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}        
       
