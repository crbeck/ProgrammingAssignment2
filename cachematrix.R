## The functions below are efficiently caching the inverse of a matrix instead of
## computing it repeatedly.

## The makeCacheMatrix function Sets the value of the matrix and gets 
## the value of the matrix. It also sets the inverse of the matrix and will get
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setmatinv <- function(matinv) m <<- matinv
  getmatinv <- function() m
  list(set=set, get=get, setmatinv=setmatinv, getmatinv=getmatinv)  
  
}


## The cacheSolve function checks first to see if the inverse has been calculated 
## and cached. If it has been calculated, then the function will return the inverse
## of the matrix, else it will calculate the inverse and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setmatinv(m)
  m
  
}
