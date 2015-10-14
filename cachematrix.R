## Put comments here that give an overall description of what your
## functions do

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
        data <- x$get()
        i <- solve(x)
        x$setinverse(i)
        i
}

