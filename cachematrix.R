# This script contains two functions:
#	a) makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
#	b) cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#		 		   If the inverse has already been calculated (and the matrix has not changed), 
#				   then the cachesolve should retrieve the inverse from the cache.
#

## This function allows users, given an input matrix, to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    my_matrix_inverse <- NULL
    
	# this function caches the matrix to be inverted, which is passed as an input argument 
	# the input matrix is assumed to be invertible
    set.matrix <- function(new_matrix) {
        my_cached_matrix <<- new_matrix
        my_matrix_inverse <<- NULL
    }
    
	# this function returns the cached matrix
    get.matrix <- function() {
        my_cached_matrix
    }
    
	# this function caches the matrix iverse, which is passed as an input argument
    set.inverse <- function(computed_inverse) {
        my_matrix_inverse <<- computed_inverse
    }
    	
	# this function returns the cached matrix iverse
    get.inverse <- function() {
        my_matrix_inverse
    }
    
	# In the end, the makeCacheMatrix function returns a list containing the above defined functions
    list(set.matrix = set.matrix, get.matrix = get.matrix,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

# This function computes the inverse of the matrix X. 
# If the inverse was already computed then the cached result is returned. 
cacheSolve <- function(x, ...) {
	
	# checking if the matrix inverse was already calculated
    inverse <- x$get.inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
	# getting the cached matrix 
    my_matrix <- x$get.matrix()
    # calculating the matrix inverse
	inverse <- solve(my_matrix, ...)
    # caching the matrix inverse
	x$set.inverse(inverse)
	# returning the matrix inverse
    inverse
}