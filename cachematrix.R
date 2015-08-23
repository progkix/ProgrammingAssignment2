## Put comments here that give an overall description of what your
## functions do

## This function, makeCacheWrite, creates a special vector - a list containing a function to:
##
##     set the value of the vector
##     get the value of the vector
##     set the inverse of the matrix
##     get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
	m <- NULL

	set <- function(y)
	{
		x <<- y

		m <<- NULL
	}

	get <- function() x

	setinverse <- function(solve) m <<- solve

	getinverse <- function() m

	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function, cacheSolve, calculates the inverse of a matrix returned
## by the makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not, the cacheSolve will simply return the cached
## inverse. If the inverse has not been calculated or the matrix has changed,
## the inverse will be calculated, cached, and finally returned.
##

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()

	if (!is.null(m))
	{
		message("returning cached inverse")

		return(m)
	}

	data <- x$get()

	m <- solve(data)

	x$setinverse(m)

	m
}

## This function, matEqual, determines if two matrices are equal and returns TRUE or FALSE
##

matEqual <- function(x, y)
{
	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}
