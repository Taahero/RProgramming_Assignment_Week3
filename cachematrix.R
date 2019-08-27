## The function created is responsible first for creating an inverse Matrix
## The function will than cache the inverse Matrix so that when called upon again it does not re-calculate the inverse matrix
## but rather retrieve the cached matrix, this optimizes the process especially when working caching large matrices



makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  #set the value of the input Matrix
  setMatrix <- function(y) {
    x <<- y                                                     # Assigning the value to an object that is outside the function's environment
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x                                     #get the value of the Matrix as assigned in the upper function
  setInverse <- function(inverse) inverseMatrix <<- inverse     #set the value of the inverse matrix
  getInverse <- function() inverseMatrix                        #get the value of the inverse matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}

## The cachesolve function is responsible for firstly checking if the inverse matrix exists as a cached matrix
## If the inverse matrix is not null it will than provide a message stating "Retrieving inverse matrix"
## The function will than instead of calculating the inverse matrix, retrieve the inverse matrix from cache
## if the inverse matrix is null, the function will than calculate the inverse matrix by using the solve function
## In order to cache the inverse matrix, the cachesolve function first needs to run, 
## on the second run the inverse matrix is retrieved from cache and not re-calculated

cacheSolve <- function(x, ...) {
  
  #get the value of the inverse matrix from the makeCacheMatrix function
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {                                 #if inverse matrix is not NULL
    message("Retrieving inverse Matrix")                        #Type message: Getting Cached Invertible Matrix 
    return(inverseMatrix)                                       #return the invertible matrix
  }
  
  #if inverse matrix is NULL then  
  MatrixData <- x$getMatrix()                                   #get the original Matrix Data 
  inverseMatrix <- solve(MatrixData, ...)                       #use solve function to inverse the matrix
  x$setInverse(inverseMatrix)                                   #set the inverse matrix 
  return(inverseMatrix)                                         #return the inverse matrix
  
  ## Returns the inverse matrix as calculated by the solve function
}

