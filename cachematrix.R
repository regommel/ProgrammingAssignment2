## assignemtn#2 - 
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## creates a matrix that will calculate its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## before re-calc - check if it already exists

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("looking for cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
