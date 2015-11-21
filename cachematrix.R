## The first function creates an object, which is a list of methods (or functions) that allows you to
## cache the input (in this case a matrix) with set(x), take the element out of the cache with get(), set and get the
## inverse of the input matrix to and from the cache with the corresponding functions.
## The second function take as input an object created with the first function and allows you to get or set the inverse
## of the matrix contained in that object and to cache it.

## This function takes as input a classical matrix. It then initalises 4 methods, which allows you to cache or take
## out from the cache both the matrix itself and its inverse calculated with the other function of this Assign.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) xinv <- inverse
    get_inverse <- function() xinv
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function takes a object "CacheMatrix" as input and return the "inverse" element of the list. If no inverse 
## element is present, it brings the matrix contained in the object into the working environment, calculate the 
## inverse with the solve() function and return its value. It also store the new calculated value into the cached obj.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$get_inverse()
    if(!is.null(xinv)) {
        message("Getting cached inverse matrix")
        return(xinv)
    }
    data <- x$get() ## Gets the matrix from the cache and stores it in the working environment
    xinv <- solve(data,...) ## Performs the inverse of a square-invertible matrix with the built-in R function
    x$set_inverse(xinv) ## Caches the matrix inverse
    xinv
}