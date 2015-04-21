## The first function creates the matrix object that can store its inverse in cache.
## First, set the value of the vector
## Second, get the value of the vector
## Third, set the value of the inverse vector with Solve function
## Fourth, get the value of the inverse vector 
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function will compute the inverse of the matrix stored by the first 
## function. If it has already been calcuated and there were no changes, then the 
## inverse values will be returned from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
