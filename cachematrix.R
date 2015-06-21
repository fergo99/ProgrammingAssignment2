## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(my_x = matrix()) {

  my_inv <- NULL
  set <- function(my_y){
    my_x <<- my_y
    my_inv <<- NULL
  
  }

  get <- function() my_x
  setinverse <- function(my_inverse) my_inv <<- my_inverse
  getinverse <- function() my_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(my_x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  my_inv <- my_x$getinverse()
  if(!is.null(my_inv)) {
      message("getting cached data")
      return(my_inv)
    
    
  }
  
  my_data <- my_x$get()
  my_inv <- solve(my_data, ...)
  my_x$setinverse(my_inv)
  my_inv
  
  
}
