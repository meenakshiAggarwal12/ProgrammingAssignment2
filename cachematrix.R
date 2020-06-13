## this function creates a special "matrix" object that can cache its inverse.
## 



makeCacheMatrix <- function(x1 = matrix()) {
    i <- NULL
  set <- function(y1) {
          x1 <<- y1
          i <<- NULL
  }
  get <- function() x1
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x1, ...) {
  i <- x1$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x1$get()
  i <- solve(data, ...)
  x1$setinverse(i)
  i
}

        

