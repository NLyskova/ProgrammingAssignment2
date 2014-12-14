## Caching the Inverse of a Matrix.
# The following pair of functions caches the inverse of a matrix 
# assumimg that the matrix supplied is always invertible.

# Step 1. Create a special "matrix" object that can cache its inverse.
# The 'makeCacheMatrixset' function creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #set new matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #get the matrix
  get <- function() x
  #set the inverse
  setinv <- function(inv) inverse <<- inv
  #get the inverse
  getinv <- function() inverse
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


##  Step 2. Compute the inverse of the special "matrix" object returned by the 'makeCacheMatrix' function. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the 'cacheSolve' function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  # Check whether the inverse has already been calculated 
  if(!is.null(inverse)) {
    message("getting cached data")
    # Return he inverse from the cache
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse   
}

##Example:
# > abc.ini<-matrix(c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)
# > abc<-makeCacheMatrix(abc.ini)
# > abc$get()
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
## Run 'cacheSolve' function for the first time
# > cacheSolve(abc)
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
## Run 'cacheSolve' function for the first time
# > cacheSolve(abc)
# getting cached data
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
## Check that the output matrix is the inverse of the matrix supplied.
## We should get the identity matrix.
# > abc.ini %*% cacheSolve(abc)
# getting cached data
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1


