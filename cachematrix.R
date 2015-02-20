## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix", which is actually
## a list contatining a function to: 
## 1. set the value of the matrix (object)
## 2. get the value of the matrix (object)
## 3. set the value of the inverse of the matrix (Object) (assume invertible)
## 4. get the value of the inverse of the matrix (Object) (assume invertible)
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
         x <<- y
         inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates/solves the inverse of the "matrix"(object)
## created with the above function. It first checks to see if the inverse
## has already been calculated. If yes, it get the inverse from the "cache" 
## or the "environment" in which object inverse was stored. Otherwise, the 
## function calculates the inverse of the matrix and sets the value of inverse
## in the "cache" via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
