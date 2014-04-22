## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of 4 functions (set, get, setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
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
# If one is already created, the function will return that inverse matrix.  If it has not yet been
# created, it will calculate the inverse matrix and store it in the makeCacheMatrix.

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