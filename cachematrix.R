## Coursera ssignment 2:
## Performed on: 7/1/2019

## Write a short comment describing this function
###This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
###This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
###If the inverse has already been calculated (and the matrix has not changed), 
###then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
