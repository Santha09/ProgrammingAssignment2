## This function does the inverse of a matrix and cache the value to avoid repetitive processing.
## if the matrix is same and the inverse of it is already done and the value is in the cache, then the cache value is returned.

## The makeCacheMatrix function, caches the input and the inverse output value.

makeCacheMatrix <- function(x = matrix()) {
  Invmat = NULL
  setMatrix <- function(y){
    x <<- y
    Invmat <<- NULL
  }
  getMatrix <- function() x
  setInvmat <- function(Invmatrix) Invmat <<- Invmatrix
  getInvmat <- function() Invmat
  list (setMatrix = setMatrix, getMatrix = getMatrix,
          setInvmat = setInvmat, getInvmat = getInvmat)
}


## The cacheSolve, checks if the inverse of the input already exists in cache and if so, 
##it returns from cache, else it inverse the data, cache it and then return the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     Invmat <- x$getInvmat()
     if (!is.null(Invmat)){
       message("getting cached matrix")
       return(Invmat)
     }
     else {
       matrixd <- x$getMatrix()
       inverse <- solve(matrixd,...)
       x$setInvmat(inverse)
       Invmat <- x$getInvmat()
       Invmat
     }
}


