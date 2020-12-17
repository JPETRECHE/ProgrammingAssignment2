## These functions cache the inverse of a (square) matrix

## this first function creates a special "matrix" (which is really a list) to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        if(dim(x)[1]!=dim(x)[2] || min(dim(x))<2) { 
                print("not square or number of rows (or columns) less than 2")
                return()
        }
        m <-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse matrix and sets
## it in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
