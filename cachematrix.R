## Calculating the inverse of a matrix could be very costly computationally speaking. 
## Hence in the folowing two functions we will cache the inverse of a matrix and 
## call it when needed instead of computing it each time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
       	 	if(!is.null(m)) {
             	   message("getting cached data")
               	   return(m)
        		}
        		data <- x$get()
        		m <- solve(data, ...)
        		x$setinverse(m)
        	
		m


}