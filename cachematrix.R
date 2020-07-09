## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The aim of this work is to write a pair of functions that cache the inverse of a matrix.
## The function "makeCacheMatrix" generate a “matrix” object which cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## The function "cacheSolve" computes the inverse of the matrix provided by the function "makeCacheMatrix" that 
## was ranned before. 

## Return a matrix that is the inverse of 'x':
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Just to check:
test_matrix <- makeCacheMatrix(matrix(2:5, 2, 2))
test_matrix$get()
##       [,1] [,2]
## [1,]    2    4
## [2,]    3    5
test_matrix$getinverse() #### Expected: NULL
## NULL

cacheSolve(test_matrix) #### getting cached data
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
