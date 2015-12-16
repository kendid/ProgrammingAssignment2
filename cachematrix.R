## this file contains a pair of functions which provide a matrix "object" 
##   (CacheMatrix), which contains a matrix and its inverse
## the CacheMatrix will be initiated by calling makeCacheMatrix() with the
##   matrix as argument. Alternatively it can be set by calling the $set() 
##   function
## the inverse will be created by calling the cacheSolve() function and is not
##   available before

# makeCacheMatrix(x) will create a CacheMatrix object, which contains
#   the matrix x and its inverse once calculated
# it provides the following functions:
#  - get(): returns the given matrix x
#  - set(y): sets the matrix of the object to the new matrix y
#  - getinverse(): returns the inverse matrix of x, if available (otherwise NULL)
#  - setinverse(y): sets the inverse matrix of x (without checking if it's correct!)

makeCacheMatrix <- function(x = matrix()) 
{
    mMatrix <- NULL
    
    set <- function(y)
    {
      x <<-- y
      mMatrix <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) mMatrix <<- inverse
    
    getinverse <- function() mMatrix
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve(x) returns the inverse of the matrix of the CachedMatrix object x
#   if it was not cached, it will be calculated and set in x
# There is no error checking for invertibility or dimensions

cacheSolve <- function(x, ...) 
{
    mInverse <- x$getinverse()
    
    # return inverse matrix if available
    if (!is.null(mInverse))
    {
      message("getting cached data")
      return(mInverse)
    }
  
    # compute inverse of matrix, set it and return
    data <- x$get()
    mInverse <- solve(data)
    x$setInverse(mInverse)
    mInverse
}
