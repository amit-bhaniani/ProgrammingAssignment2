## this program is to write two functions: makeCacheMatrix and cacheSolve

## function makeCacheMatrix - set the matrix and get the value of this matrix
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

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
        ## Return a matrix that is the inverse of 'x' from cache
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        ## Calculate the inverse of the matrix if it does not exist in the cache
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}