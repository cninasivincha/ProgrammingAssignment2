## The function is used to store data in the cache and be able to use it in the future

makeCacheMatrix <- function(x = matrix()) {
#It takes the matrix as input data and returns a list of the methods used at the time of the cache inversion
  j <- NULL   # is the created temporary array
  set <- function(y){  # the matrix that was entered is stored
    x <<- y
    j <<- NULL
  }
  get <- function()x #create the stored array
  setInverse <- function(inverse) j <<- inverse #stores the inverted matrix
  getInverse <- function() j #create the inverted matrix again
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function verifies that if the inverted matrix already exists in the beginning, if it is the case, it returns. Otherwise it is calculated and stored

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse() # obtaining the inverse matrix
  if(!is.null(j)){  #cache in array
    message("getting cached data") 
    return(j) #Returns the cache array and terminates the function
  }
  mat <- x$get() # the matrix is obtained
  j <- solve(mat,...) # the matrix inversion is calculated
  x$setInverse(j) # the investment is stored
  j #done
}
