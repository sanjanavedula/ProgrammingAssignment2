## Put comments here that give an overall description of what your
## functions do

##This function makeCacheMatrix gets a matrix as an input,sets the value of  matrix,
##gets the value of matrix,sets the inverse Matrix and gets the inverse Matrix.

## <<- operator is used to assign a value to an object,in an environment that is different 
##from the current working environment that the user is using.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            ##initialize inv as NULL,it will hold value of matrix inverse. 
  
  set <- function(y) {                  ##define the set function to assign new. 
    
    x <<- y                            ##value of matrix in parent environment.
    
    inv <<- NULL                       ##if there is a new matrix, reset inv to NULL.
  }
  
  get <- function() x               ##define the get fucntion - returns value of the matrix argument.
  
  setinverse <- function(inverse) inv <<- inverse   ##assigns value of inv in parent environment.
  
  getinverse <- function() inv                     ##gets the value of inv when called.
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)  
  ## we need this in order to refer to the functions with the $ operator.
  
}


## Write a short comment describing this function

## This function computes the inverse of the"matrix" created by,
## makeCacheMatrix function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}