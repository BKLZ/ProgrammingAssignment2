## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to 
##	(1) set the value of the matrix: get()
##	(2) get the value of the matrix: set()
##  (3) set the value of the inverse of the matrix: setinv()
##  (4) get the value of the inverse of the matrix: getinv()


makeCacheMatrix <- function(X = matrix()) {
        invX <- NULL
        set <- function(y) {
                X <<- y
                invX <<- NULL
        }
        get <- function() X
        setinv <- function(myInv) invX <<- myInv
        getinv <- function() invX
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix. 
## It first checks if the invserse has already been calculated. If so, it retrieves the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(X, ...) {
        invX <- X$getinv()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- X$get()
        invX <- solve(data, ...)
        X$setinv(invX)
        invX
}
