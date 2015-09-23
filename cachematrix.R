## import the MASS library for use to calculate the inversion when the matrix is not square
library("MASS")

## This function creates a "special" matrix that is exyend the object by having internal member
## to hold and cache the inverse of the matrix data
makeCacheMatrix = function(matx = matrix()) 
{
  ## this member contain the inversion matrix.
  inverseMat = NULL

  ## set a new data into this matrix object
  setMatrix = function(matrixData) 
  {
    matx <<- matrixData

    ## if a new data is set, we need to re-calculate the inversion. so assign it to null
    inverseMat <<- NULL
  }

  ## Get the matrix raw data
  getMatrix = function()
  {
    matx
  }
  
  ## set the inversion of this matrix data
  setMatrixInversion = function(inverseMatrix)
  {
    inverseMat <<- inverseMatrix
  }
  
  ## return the current cached matrix inversion
  getMatrixInversion = function()
  {
    inverseMat
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setMatrixInversion = setMatrixInversion,
       getMatrixInversion = getMatrixInversion)
}

## This function calculate a matrix inversion and store it/cached in the special matrix object
cacheSolve = function(matx, ...) 
{
  ## Try to get the inversion matrix
  invert = matx$getMatrixInversion()
  
  ## Check if not null - no need to re-calculate. 
  if (!is.null(invert))
  {
    message ("return the cached inversion matrix")
    return(invert)
  }
  
  ## if here - means we didn't calculate yet the inversion for this specific matrix data
  ## calculate the inversion
  
  newInvert = matrix()
  ## If the matrix is a squre matrix - we can use the solve()
  if (nrow(matx$getMatrix()) == ncol(matx$getMatrix()))
  {
    newInvert = solve(matx$getMatrix())
  }
  else
  {
    ## use the MASS library and ginv()
    newInvert = ginv(matx$getMatrix())
  }
  
  ## set the inversion into the special matrix
  matx$setMatrixInversion(newInvert)
  
  message("return the inversion of this matrix - calculated")
  return (newInvert)
}

