## The following two functions are designed to put a matrix in a cache, then solve for its inverse
## only if the inverse matrix is not already in the cache

## This function creates a list that sets and gets the values of both the initial matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
  x <<- y
  i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set=set,
     get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}

## This function computes the inverse of the matrix in the above function
## If the inverse has already been calculated, it will instead retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)){
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- solve(data,...)
      x$setinverse(i)
      i
}
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)