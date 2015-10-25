## These functions are part of the assigment 2 of Coursera's course 
## "R Programming" course. They serve the purpose of using lexical 
## scoping to avoid recalculate costly values, in this particular
##case, the inverse of a matrix

## makeCacheMatrix function creates a list of functions that have the
## purpose of setting and getting de values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) InvMat <<- inverse
  getinverse <- function() InvMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function takes the list of functions from makeCacheMatrix
## and with its help, either calculates the inverse and chache it, or
## gets the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of a matrix
  InvMat <- x$getinverse()
  if(!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  data <- x$get()
  InvMat <- solve(data)
  x$setinverse(InvMat)
  InvMat
}
