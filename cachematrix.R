## The purpose of the folowing functions is cache the inverse 
## of a matrix 

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrice <- x$get()
  inv <- solve(matrice)
  x$setinv(inv)
  inv
}
