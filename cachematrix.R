## Put comments here that give an overall description of what your
## functions do
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to: 
## set the value of the vector;get the value of the vector;set the value of the mean;get the value of the mean
## The second function calculates the inverse matrix of the special "matrix" created with the above function and
## sets the value of the inverted matrix in the cache via the setinverse function.

## Write a short comment describing this function
## This function initializes objects, defines the "behaviors" or functions for objects of type makeCacheMatrix(),
## and creates a new object by returning a list().
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## Write a short comment describing this function
## This function attempts to retrieve a inverted matrix from the makeCacheMatrix object passed in as the argument. 
## by calling the getinverse() function on the input object.
## Then it checks to see whether the result is NULL. Since makeCachematrix() sets the cached reverted matrix to NULL 
## whenever a new vector is set into the object, if the value here is not equal to NULL, cached inverted matrix will be returned to the parent environment.
## If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, calculates a solve(), uses the setinverse() function
## on the input object to set the inverse of the input matrix, and then returns the inverted matrix to the parent environment.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}