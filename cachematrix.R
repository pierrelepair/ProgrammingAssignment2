## Create a special matrix object
## with ability to cache its inverted result


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


## Invert a special matrix object by first checking if inverted matrix was 
## previously cached

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
