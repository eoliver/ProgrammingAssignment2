# Programming Assignment 2
# Author: Emerson Maurício de Oliveira
# Date: Friday, September 30, 2022

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve retrieve the inverse of a matric returned by makeCacheMatrix above from the cache that has already been calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:

## x = rbind(c(3, -4), c(-8, 2))
## m = makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
## [1,]    3   -4
## [2,]   -8    2
## cacheSolve(m)
##             [,1]       [,2]
## [1,] -0.07692308 -0.1538462
## [2,] -0.30769231 -0.1153846
