## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
makeCacheMatrix <- function(x = matrix()) {
  # variable inv, which will hold the inverted matrix, is initialized here 
  # within scope of makeCacheMatrix. identical_input is a flag to track if  
  # the user attempts to compute the inverse of the same matrix as the previous one
  
  inv <- NULL
  identical_input <- FALSE
  
  # internal variable x, the matrix we want to invert, is passed as the arg to a setter function,
  # making sure the inverse is nulled when we are passing a y not identical to the previous y.
  # assign these within the scope of the parent function.
  # tell user when they are passing an identical matrix, if so do not invalidate cache.
  
  set <- function(y) {
    if(identical(x, y)){
      set_identical_input(TRUE)
      message("input matrix is identical to cached matrix")
    }
    else{
      set_identical_input(FALSE)
      x <<- y
      inv <<- NULL
    }
  }
  get <- function() x
  
  # superassignment operator assigns val of variable inv, writing over the prev null value
  # within the scope of the makeCacheMatrix function thereby saving state
  setinv <- function(x.inverse) inv <<- x.inverse
  getinv <- function() inv
  
  # setter and getter functions for the identical_input flag
  set_identical_input <- function(true_false) identical_input <<- true_false
  get_identical_input <- function() identical_input
  
  #return a list of handles to the internal functions
  list(set_matrix = set, get_matrix = get, setinv = setinv, getinv = getinv, identical_input = get_identical_input)
}

cacheSolve <- function(x, ...) {
  #retrieve the inv value from the object passed to cacheSolve
  #if it exists, return inv , the inverse of the input matrix 
  #tell the user if the cache is being retrieved or if the input matrix is unchanged
  inv <- x$getinv()
  if(x$identical_input() && !is.null(inv)){
    message("input matrix is unchanged: getting cached data")
    return(inv)
  }
  else if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #otherwise invert the input matrix and set this value on the input object
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  #i never comment this much in real life!
}