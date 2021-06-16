## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function creates a vector (matrix) containing a function
## which is able to cache its own inverse.
## We're also assigning values to both "x" and "inv" in an environment they 
## don't belong to.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      # after setting and getting the values of the vector "x" 
      # we'll set and get the value of the "inverse" below:
      
      setInverse <- function(runMatrix) inv <<- runMatrix
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the outcome coming from the 
## "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      ## The "if" condition below checks whether the inverse has been
      ## obtained, and if yes, the function yields the inverse from 
      ## the cache and skips the  computation. 
      ## Otherwise, it calculates the inverse of the data and 
      ## sets the value of the inverse in the cache via the "setinverse" function.
      
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
            
      }
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      inv   
}
