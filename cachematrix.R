
## The first function, makeCacheMatrix creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
 n <- NULL
 set <- function(y){
  x <<- y
  n <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) n <<- inverse
 getinverse <- function() n
 list(set= set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}



## The following function computes the inverse of the special "matrix" created with the above function. 
## If the inverse has already been calculated the cachesolve retrieves the inverse from the cache and skips the computation.
## Otherwise, it ccomputes the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
 n <- x$getinverse()
 if (!is.null(n)){
  message("getting cached data")
  return(n)
 }
 data <- x$get()
 n <- solve(data, ...)
 x$setinverse(n)
 n
}




