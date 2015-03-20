## caches the inverse of a matrix

## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y) {
        ## stores values in parent environment
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x ## returns x
    setinv <- function(inverse) m_inverse <<- inverse ## stores value in parent environment
    getinv <- function() m_inverse ## returns matrix inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## computes the inverse of the special matrix
## if inverse has already been calcuated
## retrieves inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$getinv()
    if(!is.null(m_inverse)) {
        message("getting cached data")
        return(m_inverse)
    }
    ## calculates inverse if it is not in cache
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinv(m_inverse)
    m_inverse

}
