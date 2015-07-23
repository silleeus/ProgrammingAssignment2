## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) M <<- matrix
        getmatrix <- function() M
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)    
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getmatrix()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setmatrix(M)
        M
}
