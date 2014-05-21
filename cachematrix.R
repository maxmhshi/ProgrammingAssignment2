## Put comments here that give an overall description of what your
## functions do

## This function is to make a spicial matrix,
## which is a list containing functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setinv - set the value of the inverse of the matrix
## getinv - get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## initialize invx
    invx <- NULL
    ## function to initialize cached matrix
    ## and its inverse matrix
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    ## function to get cached matrix
    get <- function() x
    ## function to set inverse matrix to cache
    setinv <- function(inv) invx <<- inv
    ## fucntion to get cached inverse matrix
    getinv <- function() invx
    ## return a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function is to calculate inverse of a matrix
## which is created by makeCacheMatrix. The function
## will first get the cached value of the inverse matrix,
## if the cached value is not null, it will return
## the cached value and stop calculating, otherwise
## the function will calculate the inverse value of
## the matrix and set the inverse matrix to cache by
## setinv function.

cacheSolve <- function(x, ...) {
        ## get cached inverse matrix
        invx <- x$getinv()
        ## check if the cached value is null
        if(!is.null(invx)) {
            ## if cached value is not null, return it
            message("getting cached data")
            return(invx)
        }
        ## get cached matrix
        data <- x$get()
        ## calculate inverse matrix
        invx <- solve(data, ...)
        ## set the inverse matrix to cache
        x$setinv(invx)
        ## Return a matrix that is the inverse of 'x'
        invx
}
