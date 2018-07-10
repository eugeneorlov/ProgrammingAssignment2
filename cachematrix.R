# Matrix inversion is usually a costly computation.
# There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setinverse(m_inverse)
  m_inverse
}
