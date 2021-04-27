## This pair of functions allows you to store the inverse of a matrix so
## it can be called in the future without having to be recalculated


## makeCacheMatrix initializes the function argument as x,
##  sets the value of m to NULL and defines four other functions
## the four other functions are stored in a named list so they can easily be retrieved



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks to see if there is a stored value of m, the inverse matrix
## if there is it returns it, if it is NULL it calculates it, stores it, and then prints it

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



T <- matrix(1:4, 2, 2)
mymatrix <- makeCacheMatrix(T)
cacheSolve(mymatrix)
