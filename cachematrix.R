# To reduce computation cost of repeatedly computing the inverse of a matrix, 
# you can take advantage of caching the result.
# By using the following 2 functions you can do so: Cache the inverse of a Matrix.

# The first function makeCacheMatrix create a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      # Set the value of the matrix
      set <- function(y) {
            x <<- y
            s <<- NULL    
      }
      
      # Get the value of the matrix
      get <- function() x
      
      # Set the value of the inverse of the matrix
      setInverse <- function(solve) s <<- solve
      
      # Get the value of the inverse of the matrix
      getInverse <- function() s
      
      # Make the list of the functions above
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
      
}


# The function cacheSolve returns the inverse of the matrix
# It test if the inverse of the matrix is already calculated.
# If it has been calculated this result is returned, 
# with as message that this result is cached.
# If no previus result has been calculated, the function calculates the inverse of the matrix
# and use the setInverse function to add the result to the cache 
# and finaly returns the in inverse of the matrix.

cacheSolve <- function(x, ...) {
      
      # Look for a result in the cache
      s <- x$getInverse()
      
      # Test if a result exist. If so return message and the result from cache
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      
      # Otherwise get the value of the matrix and add it to data
      data <- x$get()
      
      # Find the inverse of the matrix
      s <- solve(data, ...)
      
      # Add the result to the cache
      x$setInverse(s)
      
      # return the inverse
      s
      
}
