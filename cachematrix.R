## The two functions below cache the inverse of a matrix

## 1. Function makeCacheMatrix: create a matrix object that could cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	# set the matrix
	set <- function(x) {
		set_matrix <<- x
		inverse <- NULL
	}
	
	# get the matrix
	get <- function() x
	
	# set the inverse
	set_inverse <- function(inverse) inv <<- inverse
	
	# get the inverse
	get_inverse <- function() inv
	
	#get the whole list
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## 2. Function cacheSolve: compute the inverse of he matrix object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
	inv <- x$get_inverse()
	
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	
	data <- x$get()	
	inv <- solve(data, ...)	
	x$set_inverse(inv)
	inv
}
