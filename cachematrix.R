## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
      set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setIm <- function(im) m <<- im
        getIm <- function() m
        list(set = set, get = get,
             setIm = setIm,
             getIm = getIm)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getIm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        c <- x$get()
        m <- solve(c) %*% c
        x$setIm(m)
        m
}
