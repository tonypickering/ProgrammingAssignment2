## The following pair of functions will invert a matrix, and return the inverted matrix. 
## If the matrix has previously been inverted and cached it reads from the cache.
## Otherwise it computes the inverted matrix ussing the "solve" function.
##
## To work properly, the "makeCacheMatrix" function should be used to assign the "special
## matrix" (set of functions) to an object. That object then serves as the input argument to the "cacheSolve"
## function. 
##
## The logic and coding strategy closely follows the example functions "makevector" 
## and "cachemean" provided in the assignment example.  The "solve" function has been
## substituted for the mean function, and other variable names tweaked to reflect matrix
## inversion instead of vector mean functionality.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)     #exits function - returns previously computed inverse
  }
  #following code only executed if "i" was null (i.e. inverse not already computed and stored in cache)
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
