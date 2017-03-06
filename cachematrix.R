## Programming Assignment 2: Lexical Scoping 
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

## Creating a makeCacheMatrix object will consist of four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## Sample run:

## x <- matrix(1:4, nrow=2, ncol=2)
## matx <- makeCacheMatrix(x)
## solv <- cacheSolve(matx)
## print(solv)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## solv2 <- cacheSolve(matx)
## getting cached data
## print(solv2)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5