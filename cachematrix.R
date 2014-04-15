## These two functions are designed to be used to calculate the inverse of a matrix.
## However, instead of calling solve() directly on the matrix, they can be 
## used in conjunction in order to  cache the inverse if the matrix has not
## been changed since the last request for the inverse of the matrix.


## This function takes a matrix, and returns a list of four functions
## to get the matrix, set the matrix to a new matrix, set the inverse
## and get the inverse (note this is null if the matrix has been changed 
##since the inverse was last set)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of the matrix stored in x
## Checks first whether the matrix has had its inverse calculated since it last changed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
