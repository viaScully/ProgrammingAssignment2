## This pair of functions caches the inverse of a matrix object.
## For this assigment, it assumes for that x is an invertable matrix.


## makeCacheMatrix: creates a special object from a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## create functions to to set, get, setinverse, and getinverse of x
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  
  ## return list of functions created
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix (unless it has already been calculated since the 
## object last changed, in which case, it returns a cached version instead).
## Return a matrix that is the inverse of 'x'

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
