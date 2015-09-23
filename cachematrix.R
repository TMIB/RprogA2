makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the value of the invese matrix to NULL
  inverseMatrix <- NULL
  #declare a 'set' function to cache the matrix
  set <- function(y) {
    x <<- y
    # wipe the value of inverse of the matrix in case the matrix was changed.
    inverseMatrix <<- NULL
  }
  # gets the value of the inverse
  get <- function() x
  #calculates the inverse of non-singular matrix via the solve function
  setInverseMatrix <- function(solve) inverseMatrix <<- solve
  # gets the inverse
  getInverseMatrix <- function() inverseMatrix
  # passes the value of the function makeCacheMatrix    
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

# Function to get the cache of the matrix
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  #if the inverse exists, it gets it.
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  #if the inverse isn't cached, calculate it
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}