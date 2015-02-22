## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setInv <- function(inv) m <<- inv #set the value of the inverse
  getInv <- function() m #get the value of the inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m) #set the value of inverse
  m
}

a = matrix(1:4, 2, 2) #matrix
aCached = makeCacheMatrix(a)
aInverse = cacheSolve(aCached) #inversed
a
aInverse
