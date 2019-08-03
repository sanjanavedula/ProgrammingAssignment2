## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function makeCacheMatrix gets a matrix as an input,sets the value of the matrix,
#gets the value of the matrix, sets the inverse Matrix and also gets the inverse Matrix.

## <<- operator is used to assign a value to an object in an environment 
##that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             
  
  set <- function(y) { 
    
    x <<- y
    
    inv <<- NULL
  }
  
  get <- function() x  ##gets the value of the Matrix
  
  setInverse <- function(inverse) inv <<- inverse  ##sets the value of the invertible matrix
  
  getInverse <- function() inv  ##gets the value of the invertible matrix
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

## This function computes the inverse of the matrix created by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {     
    
    message("getting cached data")
    
    return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}
