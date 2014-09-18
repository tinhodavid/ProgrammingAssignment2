## This assignment 2 of Coursera class - R programming
## Two functions that are used to create a special object 
## that stores an inverse of a matrix and cache's its inverse.

## This function creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initial assigning m to be NULL
  m <- NULL
  
  ## define set func
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## define get func
  get <- function() x
  
  ## define setsolve func
  setsolve <- function(solveVal) m <<- solveVal
  
  ## define getsolve func
  getsolve <- function() m
  
  ## create special 'Vector' containing 4 functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the inverse of 'x'
  m <- x$getsolve()
  
  ## Check if inverse of 'x' is calculated before
  if(!is.null(m)) {
    ## Yes and return the cache value
    message("getting cached data")
    return(m)
  }
  
  ## No and compute the inverse through solve() 
  ## Get the matrix data
  data <- x$get()
  
  ## Compute by using solve()
  m <- solve(data, ...)
  
  ## Cached the inverse of a matrix
  x$setsolve(m)
  
  ## Return a matrix that is the inverse of a matrix
  m
}
