## this program is to write two functions: makeCacheMatrix and cacheSolve

## function makeCacheMatrix - set the matrix and get the value of this matrix

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(minverse) minv <<- minverse
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## function cacheSolve - calculate inverse of the matrix 
## or print cached inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}