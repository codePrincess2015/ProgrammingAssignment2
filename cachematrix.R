## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix, which is a list containing functions  
## to the value of the matrix, get the value of the matrix, set the value of the 
## inverse, and get the value of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the 
## mean fro mthe cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
     message("getting cached data")
     return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
