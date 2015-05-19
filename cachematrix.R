## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##             If the inverse has already been calculated (and the matrix has not changed),
##             then the cachesolve should retrieve the inverse from the cache.
## Assumption: matrix supplied is always invertible


## creates a special "Matrix", which is really a list containing a function to
## set(): sets the value of the Matrix and inv will be set NULL
## get(): gets the value of the Matrix
## setsolve(): the value of inv is set to the inverted matrix of x
## getsolve(): get the value of the inverted matrix of x
## inv will be NULL if setsolve() is not called since instantiation/set() of makeCacheMatrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL   
  }
  get <- function() x
  setsolve <- function() inv <<- solve(x)
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## returns the inverse of the special "matrix" returned by makeCacheMatrix
## it first checks to see If the inverse has already been calculated.
## If so, then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it sets the value of the inv in the cache via the setsolve function.
cacheSolve <- function(x, ...) {        
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  x$setsolve()
  inv <- x$getsolve()
  ## Return a matrix that is the inverse of 'x'
  inv
}
