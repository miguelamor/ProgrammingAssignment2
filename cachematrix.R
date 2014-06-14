## makeCacheMatrix creates a "special matrix", which is a list
## that contains different functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve #solve(m) inverse of matrix m
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
}


## calculate the inverse of the "special matrix" created
## with the above function (makeCacheMatrix).
## It first checks to see if the inverse has already been calculated, 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
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
