## Put comments here that give an overall description of what your
## functions do
#
#  This pair of functions is used to cache a matrix and its inverse and to retrieve
#  the cached values.  Here is an example of how to use them:
# 
# > lst <- makeCacheMatrix(a_matrix) creates a list stored as lst containing the 
#       functions necessary to compute the inverse of a_matrix or retrieve it from cache
# > inverse <- cacheSolve(lst) stores the the inverse of a_matrix in inverse, either from cache
#       if was previously computed, or as computed by the solve function, if it was not
# 
# Now if another_matrix is a different matrix to invert, you can proceed as above
# to compute its inverse, but you don't need to re-run makeCacheMatrix to do it.  
# If you already have lst from a previous run of makeCacheMatrix, 
# then the inverse can be obtained as follows:
#
# > lst$set(another_matrix)
# > inverse <- cacheSolve(lst)
#
# inverse is now the inverse of another_matrix

## Write a short comment describing this function

# makeCacheMatrix creates a list of functions as follows:
#       set places a matrix, x, in cache and sets its inverse to NULL.  This function
#               is not used in cacheSolve but could be used to change (re-set) the matrix to be
#               inverted instead of re-calling makeCacheMatrix.  Then cacheSolve could be used to 
#               calculate the inverse of that different matrix (see example above)
#       get gets the matrix from cache
#       set_inverse places the inverse of the matrix, xinv, in cache
#       get_inverse gets the inverse of the matrix, xinv, from cache

makeCacheMatrix <- function(x = matrix()) {
        xinv = matrix()
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) xinv <<- inverse
        get_inverse <- function() xinv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function

#  cacheSolve returns the inverse of the matrix x, either computing it,
#  or getting it from cache.  Its arguments are a list of functions for getting
#  data from cache and a list of arguments that could be passed to "solve", using
#  the "..." argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # x is a list of functions for setting and getting "x" and its inverse, "xinv"
        #
        xinv <- x$get_inv()
        if (!is.null(xinv)) {
                message("getting cached inverse")
                return(xinv)
        }
        matrix <- x$get()
        xinv <- solve(matrix,...)
        x$set_inverse(xinv)
        xinv
        
}


