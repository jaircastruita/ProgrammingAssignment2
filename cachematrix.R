## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv <- NULL ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) { 
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
    x <<- y
    inv <<- NULL ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x ## define the get function - returns value of the matrix argument
  setinv <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
  getinv <- function() inv ## gets the value of inv where called
  list(set = set, get = get, ## you need this in order to refer to the functions with the $ operator
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getinv()
  # if the inverse has already been calculated
  if(!is.null(inv)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  inv
}
