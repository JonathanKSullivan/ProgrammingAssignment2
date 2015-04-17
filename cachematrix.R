##  Below are two functions that are used to create a special 
## object that stores a matrix and caches its inverse.

## creates a special "vector", which is really a list containing a 
#function t o

#1.get the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrx) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculates the Inverse of the special "vector" created with the above 
#function. However, it first checks to see if the mean has already 
#been calculated. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  matrx$setinverse(inv)
  inv
}
