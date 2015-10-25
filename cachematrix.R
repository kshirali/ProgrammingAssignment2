## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function generates a list of private functions that can be used to 
# 1. Cast a square matrix from an input vector (with error checks)
# 2. Read the matrix
# 3. Compute its inverse

makeCacheMatrix <- function(x = matrix()) {

# Check if input vector can be recast as square matrix
  
  d <- dim(x)
  x <<- matrix(data=NA,d[1],d[1])
  matinv <<- matrix(0,d[1],d[1]) # initialize to zero matrix
  
  
  # Set the value for matrix
  # Default for inverse is set to be same as the matrix itself
  set <- function(y)
  {
    x <<- matrix(data=NA,d,d)
    matinv <<- matrix(0,d,d) # initialize to zero matrix
  }
    
  
  # Get value of vector used to make matrix
  get <- function() x
 
  # Compute the inverse of the square matrix
  setMatrixInv <- function(mat) matinv <<- solve(mat)
  
  # Access matrix for which inverse is being computed
  getMatrix <- function() x
  
  # Access inverse of the matrix
  getMatrixInv <- function() matinv
  
  # Generate the list
  list(set = set, get = get, setMatrixInv = setMatrixInv, getMatrix = getMatrix, getMatrixInv = getMatrixInv)
  

}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get matrix and display
  m <- x$getMatrix()
  d <- dim(m)
  print(m)
  print(d)
  
  # Check if matrix is the default zero matrix
  if(identical(m,matrix(data=NA,d[1],d[1])))
  {
    mat <- x$get()
    matinv <- x$setMatrixInv(mat)
    matinv
  }
  else
  {
    message("getting cached data")
    return(x$getMatrixInv())
  }
}
