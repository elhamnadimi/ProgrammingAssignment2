## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  m <- NULL
  setMat <- function(y) {
    mat <<- y
    m <<- NULL
  }
  getMat <- function() mat
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(setMat = setMat,
       getMat = getMat,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse matrix when it is found")
    return(m)
  }
  data <- x$getMat()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
#testing 
l <- matrix(1:4, 2, 2)
ca <- makeCacheMatrix(l)
cacheSolve(ca) 
cacheSolve(ca)
c <- makeCacheMatrix(-l)
cacheSolve(ca)
cacheSolve(c)