## The functions makeCacheMatrix and solveCacheMatrix allow the caching of
## matrix inversion calculations so that subsequent references to the inverse
## of a matrix doensnt have to recalculate the inverse from scratch.
##
## makeCacheMatrix is initialised with an invertible matrix. and returns a
## list of functions for setting the matrix, getting the matrix, setting the
## inverse of the matrix, and getting the inverse of the matrix.
##
## makeCacheMatrix can be used for caching the results of a matrix inversion
## so that when code needs to use the inverse again it can use the cached
## copy if there is one, rather than doing the full recalculation.
##
makeCacheMatrix <- function(x = matrix()) {
    ## The matrix inverse is initialised to NULL.
    inverse <- NULL

    ## Re-initialize the makeCache matrix data structure with a new matrix,
    ## and NULL out it's inverse.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## Return the matrix that the makeCacheMatrix data structure is caching.
    get <- function() {
        return(x)
    }

    ## Set the cached inverse in this makeCacheMatrix to inv.
    setinverse <- function(inv) {
        inverse <<- inv
    }

    ## Return the cached inverse - either NULL, or the value set using the
    ## function setinverse(x).
    getinverse <- function(){
        return(inverse)
    }

    # makeChaceMatrix's returns a list containing the set and get functions.
    list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}


##
## Return the inverse of the invertible matrix stored in x by:
## - if x has a cached inverse, return it.
## - if there is not a cached inverse in x, calculate the inverse, store it in
##   x and return it.
#
## For big matricies, returning the cached version will be significantly faster
## for subsequent calls to cacheSolve than re-calculating the inverse.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        return(inv)
    }
    m <- x$get()
    x$setinverse(solve(m))
    return(x$getinverse())
}
