## these two functions take advantage of the scoping rules of R. See function descriptions 
## (below) for specfic descriptions of wha they do. Using cache may help to reduce cycle times
## and prevent recomputing on large datasets.

## This function creates a special "matrix" object which is a list of set, get, set the 
## inverse, or get the inverse from cache. Function requires a square matrix

makeMatrix <- function(x = matrix()) {
  ## sets the variable m_inv to empty
  m_inv <- NULL
  
  set <- function(y){
          ## assigning values to variables used outside this function.
          ## <<- means R searches parent directories for variables
          x <<- y
          m_inv <<- NULL
  }
  
  ## call the internal function and set to variable get
  get <- function() x
  
  set_inv <- function(solve) m_inv <<- solve
  get_inv <- function() m_inv
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## Uses output of makeCacheMatrix
## If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inv()
  
  ## if cache exists return result from cache and use message to indicate
  if(!is.null(m_inv)){
    message("getting cached matrix")
    return(m_inv)
  }
  
  ## if result not cached, calculate inverse and place in m_inv variable
  matrix1 <- x$get()
  m_inv <- solve(matrix1)
  
  ## caches the solved value
  x$set_inv(m_inv)
  
  m_inv
}
