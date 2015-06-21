## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
#Essentially, this function creates a list of functions (get, set, getInverse, setInverse) to aid in cacheing.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <-- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Tries to get the inverse if it has been computed already, if not it grabs the data and computes it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached matrix inverse")
    return(inverse)
  }
  matrixData <- x$get()
  inverse <- solve(matrixData)
  x$setInverse(inverse)
  inverse
}
