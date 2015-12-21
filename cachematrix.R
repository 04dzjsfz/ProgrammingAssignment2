
#Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
#The first function, makeVector creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setsolve <- function(invMatrix) m.inv <<- invMatrix
  getsolve <- function() m.inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
#The following function calculates the mean of the special "matrix" created with the above function.
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m.inv <- x$getsolve()
  #preM = x$get()
  if(!is.null(m.inv)) {
    message("getting cached data")
    return(m.inv)
  }
  data <- x$get()
  m.inv <- solve(data, ...)
  x$setsolve(m.inv)
  return(m.inv)
}
