## This function Makecachematrix gets matric as an input and sets the inverse matrix
## This function can cache its own object

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  SetMatrix <- function(y)
  {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(SetMatrix = SetMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
       
 }


##The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Invertible Matrix")   
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         
  return(invMatrix)
}


