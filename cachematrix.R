## cachematrix.R

## Author : M.H. Scheltema
## Date   : 19 april 2014
## Coursera peer assigment

## This R script contains 2 functions: makeCacheMatrix and cacheSolve

## function makeCacheMatrix is responsible for making a special kind of matrix,
## which stores the matrix in a special environment (not the global env.)

## function cacheSolve is responsible for calculating the inverse of the matrix
## which was created by the function makeCacheMatrix.

## ============================================================================


## function makeCacheMatrix
## stores a matrix in a special environment and returns a list with methods:
## - set/get for setting and getting the value of the matrix
## - setinverse / getinverse for getting and setting the inverse value of
##   the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## function cacheSolve
## calculates the inverse of a matrix which was stored by thr function
## makeCacheMatrix. The function first checks if the inverse of the
## matrix was alread claculated, if so it gets the inverse from
## the cache. If not then the inverse of the matrix is calucalted and
## the result is stored in the special environment.
##
## function assumes that the input matrix is inversable, so no validations in
## this function for checking in the matrix can be inversed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}
