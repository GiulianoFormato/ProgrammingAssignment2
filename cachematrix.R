# This script contains two functions:
#	a) makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
#	b) cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#		 		   If the inverse has already been calculated (and the matrix has not changed), 
#				   then the cachesolve should retrieve the inverse from the cache.
#

## This function allows users, given an input matrix, to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    my_matrix_inverse <- NULL
    
    set.matrix <- function(new_matrix) {
        my_cached_matrix <<- new_matrix
        my_matrix_inverse <<- NULL
    }
    
    get.matrix <- function() {
        my_cached_matrix
    }
    
    set.inverse <- function(computed_inverse) {
        my_matrix_inverse <<- computed_inverse
    }
    
    get.inverse <- function() {
        my_matrix_inverse
    }
    
    list(set.matrix = set.matrix, get.matrix = get.matrix,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

# This function computes the inverse of the matrix X. 
# If the inverse was already computed then the cached result is returned. 
cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    my_matrix <- x$get.matrix()
    inverse <- solve(my_matrix)
    x$set.inverse(inverse)
    inverse
}