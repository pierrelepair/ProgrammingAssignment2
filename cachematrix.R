## Couple of functions to create a special matrix object and invert that object


## makeCacheMatrix creates a special matrix, i.e. a list of funtion to
## set the value of the matrix, get the value of the matrix, set the value of
## the matrix's inverse and get the value of the matrix's inverse


makeCacheMatrix <- function(x = matrix()) {
                im <- NULL
                set <- function(y) {
                        x <<- y
                        im <<- NULL
                }
                get <- function() x
                setinvmatrix <- function(matrix) im <<- matrix
                getinvmatrix <- function() im
                list(set = set, get = get,
                     setinvmatrix = setinvmatrix,
                     getinvmatrix = getinvmatrix)
}


## cacheSolve calculates the inverse of a makeCacheMatrix object.
## it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## in the cache via the setinvmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                im <- x$getinvmatrix()
                if(!is.null(im)) {
                        message("getting cached data")
                        return(im)
                }
                im <- solve(x$get(), ...)
                x$setinvmatrix(im)
                im
}
