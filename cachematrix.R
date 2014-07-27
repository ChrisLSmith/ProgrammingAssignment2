#########################################################################
## Class: makeCacheMatrix
## Author: Christopher L. Smith (July 26, 2014)
## Description: OO Class to store (cache) a matrix and it's inverse 
##              so that you don't have to calculate the inverse
##              every time.  
##
## Usage Example: To use this function, use the following convention:
##
## > mymatrix <- rbind(c(1,-1/4), c(-1/4,1))
## > mm <- makeCacheMatrix(mymatrix)
## > cacheSolve(mm)
##
## > mm$get()
##   [,1]  [,2]
##   [1,]  1.00 -0.25
##   [2,] -0.25  1.00
##
## > mm$getinv()
##   [,1]      [,2]
##   [1,] 1.0666667 0.2666667
##   [2,] 0.2666667 1.0666667
###########################################################################

###########################################################################
## Function: makeCacheMatrix
## Description:  Class definition to cache a matrix and it's inverse.  
##               Allows to get and set the matrix and it's inverse.
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<-solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

###########################################################################
## Function: cacheSolve
## Description:  Runs the solve function on a cached matrix, then stores
##              it's inverse as a member of the cached matrix.
###########################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
