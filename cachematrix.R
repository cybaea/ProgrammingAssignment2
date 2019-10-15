## Functions to cache a matrix's inverse and to calculate that inverse

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setsolve <- function(value)
    m <<- value
  getsolve <- function()
    m
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## Calculate the inverse of the matrix and cache it.
## Use the cached value if possible.
## NOTE: assuming x is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
