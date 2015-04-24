# This pair of functions caches the inverse of a matrix so it can be looked up
# rather than needing to be recomputed. These functions assume the input matrix
# is invertible.

# The makeCacheMatrix takes as input an invertible matrix, 'x', and returns
# a list of functions to: 
#       1. set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of the matrix inverse
#       4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The cacheSolve function takes the output of the makeCacheMatrix function run
# on an invertible matrix, 'x', and returns a matrix that is the inverse of 'x'. 
# Before calculating the matrix inverse, it first checks to see if that has
# already been done. If so, it gets the inverse from the cache and does not redo
# the computation. If not, it calculates the inverse and sets the value of the
# inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
