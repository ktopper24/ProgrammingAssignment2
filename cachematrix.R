#This set of functions will define a "special matrix" 
#so that the inverse of the matrix passed in can
#be cached and retrieved later whithout having to recalculate
#the inverse



# This will make a "special matrix" that contains functions to:
# set a matrix of values
# retrieve this matrix
# set the inverse of the matrix
# retrieve the inversed matrix

makeCacheMatrix <- function(matrix = matrix()) {
  i <- NULL
  set <- function(y) {
    matrix <<- y
    m <<- NULL
  }
  retrieve <- function() matrix
  set.inverse <- function (inverse) i <<- inverse
  retrieve.inverse <- function() i
  list(set = set, retrieve = retrieve,
       set.inverse = set.inverse, retrieve.inverse = retrieve.inverse)
}

# This will check to see if the inverse of "matrix" has already been calclated
#If it has then it will return the value
#If it has not then it will calculate it and cache it

cacheSolve <- function(matrix) {
  i <- matrix$retrieve.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- matrix$retrieve()
  i <- solve(data)
  matrix$set.inverse(i)
  i
}