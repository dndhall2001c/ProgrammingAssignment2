## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix is used to make a special matrix that has the ability to cache the inverse of the matris
##

## Usage
##
##crete a with makeCacheMatrix framework
## > a<- makeCacheMatrix()
##create Matrix
## > a$set(1:4, 2)
##inspect Matrix
## > a$get()

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y ,z) {
          x <<- matrix(y, nrow=z)
          m <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## Write a short comment describing this function
##
## cacheSolve is used to either compute/cache/retrieve inversed matrix or just retrieve inversed matrix from cache
##

## Usage
##
## run cachesolve(a) to see if the results are cached
## > cachesolve(a)
## Run again to see cached results
## > cachesolve(a)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)
     m
}
