## Together, the two functions below can take in a square matrix, return its inverse, and store that inverse matrix
## in cache, so that if the inverse of the same matrix needs to be called, there is no need to redo the calculation.

## makeCacheMatrix returns a list of 4 functions (set, get, setinv, getinv)

makeCacheMatrix <- function(x = matrix()){ 
        ## matrix needs to be a square matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve looks to see if an inverse matrix is already created and stored in makeCacheMatrix.
## If one is already created, the function will return that inverse matrix.  If it has not yet been
## created, it will calculate the inverse matrix and store it in the makeCacheMatrix, and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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