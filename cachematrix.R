
## makeCacheMatrix creates a "matrix" object/list containing the functions 
## 1. set(y) - cache the matrix y 
## 2. get() - gets the cached matrix
## 3. setInverse(inv) - cache the inverse inv
## 4. getInverse() - gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks if the inverse has already been calculated.
## If it has, it obtains the inverse and returns it 
## If it has not been calculated, it will compute the inverse,
## return it, and cache the value via the setInverse function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  message("Computing inverse")
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
