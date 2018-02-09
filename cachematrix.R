##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
    
  }
  get <-  function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m 
  
  list(set = set, get = get,  setinverse =  setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
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


##  Some output that shows that the program is working
##> x <- rbind(c(1,-2),c(-2,1)
##  f <- makeCacheMatrix(x)
##   f$get()
##  the output is the following
##       [,1]  [,2]
##  [1,]    1   -2
##  [2,]   -2    1

## In order to calculate the inverse of the matrix 
##    cacheSolve(f)
##        [,1]       [,2]
##   [1,] -0.3333333 -0.6666667
##   [2,] -0.6666667 -0.3333333






