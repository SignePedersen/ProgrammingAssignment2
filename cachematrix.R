## Matrix inversion is usually a costly computation. 
## Thus caching the inverse of a matrix may create benefits

## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(inverse.matrix) inv <<- inverse.matrix
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The function returns the inverse of the matrix. 
## First it checks if the inverse has already been computed. 
## if "yes" - it skips the compution - if "no" it computes the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse.matrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse.matrix(data, ...)
  x$setinverse.matrix(inv)
  inv
}
