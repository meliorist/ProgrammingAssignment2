## Put comments here that give an overall description of what your
## functions do

## creates a list containing four functions, a getter, setter for the matrix and its inverse

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
         setinverse = setinverse,
         getinverse = getinverse)
}


## solves for the inverse of the matrix using solve. Checks for a non-null
## value for inv, which is the cache value that comes from outside the scope of the
## function. Returns the cache value if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}