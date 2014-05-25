# File:
#  cachematrix.R
#
# Contains a pair of functions which together implement caching for
# invertible matrices using the lexical scoping of R. makeCacheMatrix
# essentially creates an object (in the OOP sense) which holds the
# matrix and its "methods", i.e. functions that manipulate the object.
# cacheSolve takes an object created by makeCacheMatrix and computes
# the inverse of the underlying matrix using the R method `solve`.
# The first time it is called after an alteration of the matrix, it
# stores the inverse in the "cache". If the matrix is not modified
# effectively "touched" by the `set` method, cacheSolve returns the
# cached inverse.
#
# Note: 
#  The comments in this file adhere to Google's R Style Guide 
#  (http://goo.gl/sr34zp).

makeCacheMatrix <- function(x = matrix()) {
  # Creates a matrix that caches its inverse in the parent environment.
  #
  # Args:
  #  w: An invertible (square) matrix. Defaults to an empty matrix.
  #
  # Returns:
  #  The caching matrix in the form of a list with get, set, getinverse, 
  #  and setinverse methods.
  inv <- NULL
  set <- function(z) {
    w <<- z
    inv <<- NULL
  }
  get <- function() w
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  # Computes the inverse of a matrix, with caching.
  #
  # Args:
  #  a: A caching (invertible square) matrix obtained using makeCacheMatrix()
  #  b: An identity matrix of the same size as `a`.
  #
  # Returns:
  #  The inverse of `a`. If already computed, returns it from cache.
  i <- a$getinverse()
  if (!is.null(i)) {
    message("getting cache inverse")
    return(i)
  }
  mat <- a$get()
  inv <- solve(mat, b, ...)
  a$setinverse(inv)
  inv
}
