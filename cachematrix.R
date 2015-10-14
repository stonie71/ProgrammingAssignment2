## Calculates the inverse of a matrix x. Checks a cache to see if the inverse 
## has been previously calculated for the same matrix and if so, returns the
## cached value. If not found in cache, calculates the inverse and caches it. 

## Creates a matrix-like object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	setmatrix <- function(y) {
		x <<- y
		i <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = 
		setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix-like object. Uses cache where possible. 

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$getmatrix()
        i <- solve(data)
        x$setinverse(i)
        i
}
