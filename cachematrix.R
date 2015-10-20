## makeCacheMatrix and cacheSolve store an invertible matrix, calculate the
## inverse, and cache that inverse.

## makeCacheMatrix stores the functions set, get, setsolve, and getsolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {           ##stores value of matrix
    x <<- y
    m <<- NULL                   ##restores inverse value when matrix is changed
  }
  get <- function() {            ##recalls x; input from makeCacheMatrix
    x 
    } 
  setsolve <- function(solve) {  ##stores value of inverse
    m <<- solve 
  }
  getsolve <- function() {       ##recalls inverse value
    m
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)      ##stores all the functions
}

##cacheSolve searches for cached value of inverse and returns inverse,
##calculates inverse and stores inverse in makeCacheMatrix object

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()               ##verifies matrix inverse is stored 
  if(!is.null(m)) {                       ##and is not null
    message("getting cached data")
    return(m)                     ##if inverse is not null, displays message 
  }                                       ##and returns inverse
  data <- x$get()                 ##gets matrix stored by makeCacheMatrix
  m <- solve(data, ...)           ##computes the inverse
  x$setsolve(m)                   ##stores value of inverse object already created
  m
}
