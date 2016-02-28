## The makeCacheMatrix and cacheSolve functions store a matrix and caches its inverse using the generic function 'Solve'. 
## 1. Use makeCacheMatrix to store a matrix of which the inverse should be calculated
## 2. Use the output from makeCacheMatrix as the argument for cacheSolve in order to calculate the inverse matrix. When the inverse has already 
## been calculated it will get the inverse matrix from cache instead of calculating it again. 




## The makeCacheMatrix function: 
## Stores the matrix specified as the argument.
## Sets the inverse matrix to NULL (since the inverse of the new entered matrix has not yet been calculated).
## specifies the operation that needs to be executed.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <-function(y) {
    x<<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <-function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The cacheSolve function:
## Calculates and adds the inverse matrix to the output from the makeCacheMatrix. 
## When the inverse has already been calculated and stored it will simply return the stored values.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

