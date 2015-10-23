# Accept a vector as input, then convert it into square matrix
makeCacheMatrix <- function(x = c(0,0,0,0))
{
  # Check if input vector can be recast as square matrix
  mat <- matrix(x, 2, 2)
  matinv <- mat
  
  d <- sqrt(length(x))
  if(as.integer(d) != d)
  {
    print("ERROR : Matrix is not square!")
    return
  }
  else
  {
    mat <<- matrix(x,d,d)
    matinv <<- matrix(0,d,d) # initialize to zero matrix
  }
  
  # Set the value for matrix
  # Default for inverse is set to be same as the matrix itself
  set <- function(y)
  {
    d <- sqrt(length(y))
    if(as.integer(d) != d)
    {
      print("ERROR : Matrix is not square!")
      return
    }
    else
    {
      mat <<- matrix(y,d,d)
      matinv <<- matrix(0,d,d) # initialize to zero matrix
    }
    print(mat)
  }
  
  # Get value of vector used to make matrix
  get <- function() x
 
  # Compute the inverse of the square matrix
  setMatrixInv <- function(mat) matinv <<- solve(mat)
  
  # Access matrix for which inverse is being computed
  getMatrix <- function() mat
  
  # Access inverse of the matrix
  getMatrixInv <- function() matinv
  
  # Generate the list
  list(set = set, get = get, setMatrixInv = setMatrixInv, getMatrix = getMatrix, getMatrixInv = getMatrixInv)
  
}


