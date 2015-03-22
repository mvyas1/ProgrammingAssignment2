## The makeCacheMatrix function creates a matrix
## The function takes the following arguments
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The function below returns the inverse of a matrix.
## The function checks if the inverse of the matrix is already calculated.
## If it is it get the result and does not do the computation
## The function below assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}