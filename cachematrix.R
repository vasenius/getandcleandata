## This function takes a matrix as an input and returns a list
## of functions: set(), get(), setinv(), getinv()
## These do the following respectively:

# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL # initialize value of matrix inverse
## set the value of the matrix 
set <- function(y) {
  x <<- y
  inv <<- NULL
}
## get the value of the matrix
get <- function() x

## setinv
setinv <- function(inverse) inv <<- inverse
## getinv
getinv <- function() inv

list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## CachSolve checks to see if the matrix has been solved
## if if it has, it gets the solution from the cace
## Otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## check cache for inverse
  ## If an inverse is found in the cache, return that
  ## information (instead of calculating the inverse)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) 
  }
  ## If the cache doesn't have the inverse, calculate it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

