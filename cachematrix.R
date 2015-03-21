#    Functions to solve the inverse of a matrix and cache the output
#    If you enter the same matrix multiplte times, this system of functions will
#    retrieve the cached value of the inverse instead of recalculating
#    Usage:
#    your.output <- makeCacheMatrix(your.matrix)
#    your.inverted.matrix <- cacheSolve(your.output)

#    List full o' functions needed to solve for the inverse of a matrix and cache
#    both the matrix and its inverse
#    Cached variables created: matrixCached and inverseCached

makeCacheMatrix <- function(x = matrix()) 
{
     #    Initialize cached variables
     if (!exists("inverseCached"))
          inverseCached <- NULL
     
     if (!exists("matrixCached"))
          matrixCached <- NULL
     
     #    Function sets the cached value for the matrix
     setMatrixCached <- function(x2=matrix)
     {
          matrixCached <<- x2
          # If you are setting the value of a new matrix, set the value of its inverse
          # to null, as well
          inverseCached <<- NULL
     }
     
     #    Function returns the value of the entered matrix
     getMatrix <- function() x
     
     #    Function returns the value of the cached matrix
     getMatrixCached <- function() 
     {
          matrixCached
     }
     
     #    Function sets the value of the cached, inverted matrix
     setInverse <- function(inverseCalculated) 
     {
          inverseCached <<- inverseCalculated
     }
     
     #    Function returns the value of the cached, inverted matrix
     getInverse <- function() 
     {
          inverseCached
     }
     
     list(setMatrixCached = setMatrixCached, 
          getMatrix = getMatrix,
          getMatrixCached = getMatrixCached,
          setInverse = setInverse,
          getInverse = getInverse)
     
}


#    Function calls makeCacheMatrix
#    Checks to see if the current matrix has been called before and cached
#    If it was cached, it should return the cached value
#    If there is no cache, or if the matrix has changed, calculate the inverse and cache it

cacheSolve <- function(x, ...) 
{
     #    Return a matrix that is the inverse of 'x'
     #    If an inverse has already been calculated AND if the matrix
     #    has not changed, return the cached matrix
     
     #    Retrieve the cached matrix, cached inverse matrix, and entered matrix
     matrixCached.local <- x$getMatrixCached()
     inverseCached.local <- x$getInverse()
     enteredMatrix <- x$getMatrix()
     
     #    Check conditions for returning the cached inverse: cache exists, entered matrix
     #    is the same as the cached matrix
     if(!is.null(matrixCached.local) & identical(enteredMatrix, matrixCached.local))
     {
          message("Matrix has not changed -- retrieving cached inverse")
          return(inverseCached.local)
     }
     
     #    Otherwise...
     #    calculate the inverse
     inverseCalculated <- solve(enteredMatrix)
     
     #    Cache both the matrix and its inverse
     x$setMatrixCached(enteredMatrix)
     x$setInverse(inverseCalculated)
     
     inverseCalculated          
}
