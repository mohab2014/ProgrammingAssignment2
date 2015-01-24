## Programming Assignment 2:

##The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  MI <- NULL
  set <- function(y) {
    x <<- y
    MI <<- NULL
  }
  get <- function() x
  setInv <- function(inv) MI <<- inv
  getInv <- function() MI
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  


}


##The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix is done by calling the "solve" function

cacheSolve <- function(x, ...) {
  MI <- x$getInv()
  if(!is.null(MI)) {
    message("getting cached data")
    return(MI)
  }
  data <- x$get()
  MI <- solve(data, ...)
  x$setInv(MI)
  MI ## Return a matrix that is the inverse of 'x'
  
}
