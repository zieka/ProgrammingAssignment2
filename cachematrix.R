#
# cachematrix.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# Provides two functions, one which creates a special "matrix" object
# that can cache its inverse and the other which computes the inverse
# of a special "matrix" object or pulls the inverse from cache if
# available to avoid resource cost of recalculating.
#

# makeCacheMatrix(x) creates a special matrix which is really a list
# containing a function to get/set the matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL

	#sets the value of the matrix
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}

	#gets the value of the matrix
	get <- function() x

	#sets the value of the inverse
	set_inverse <- function(inverse) inverse_matrix <<- inverse

	#gets the value of the inverse
	get_inverse <- function() inverse_matrix

	#list of special functions
	list(
		set = set,
		get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse
	)
}


# cacheSolve(x,...) will check the cache for the inverse of matrix x
# If the inverse exists in cache it will return it, otherwise it will
# compute the inverse and cache it.

cacheSolve <- function(x, ...) {
	inverse_matrix <- x$get_inverse()

	#Return the inverse matrix if it has been previously cached
	if(!is.null(inverse_matrix)) {
		message("getting cached data")
		return(inverse_matrix)
	}

	#Inverse was not cached so compute the inverse and cache it
	data <- x$get()
	inverse_matrix <- solve(data)
	x$set_inverse(inverse_matrix)

	#prints out the inverse matrix
	inverse_matrix
}
