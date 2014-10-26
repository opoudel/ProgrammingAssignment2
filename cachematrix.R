## Put comments here that give an overall description of what your
## functions do

## this function takes any matrix as input and checks if matrix is invertible and if it is not then print message.
## If the matrix is invertible, it creates getter and setter methods for input matrix and inverse of it

makeCacheMatrix <- function(x = matrix()) {
  if (det(x) == 0) {
    print("Matrix is not invertible")
    return(NA)
  }
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function users makeCacheMatrix getters and setters to check if the inverse of the matrix is already in cache or not
## if the inverse of the input matrix is in cache, it returns from cache if not compute inverse of matrix using slove function, cache it and return the inverse of the input  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting inverse of matrix from cache")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
