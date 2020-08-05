## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates a special "matrix" by setting the matrix,
#getting the matrix, setting the inverse of the matrix and
#getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() {x}
  setinverse = function(inverse) {inv <<- inverse}
  getinverse = function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#this function calculates the inverse of the matrix created with 
#the makeCacheMatrix function. it checks first to see if the inverse has
#already been computed. if it has, then the function gets the inverse matrix
#from the cache and skips the computation. otherwise, it computes the inverse
#of the matrix and sets the inverse of the matrix in the cache with the
#setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix = x$get()
  inv = solve(matrix, ...)
  x$setinverse(inv)
  inv
}
