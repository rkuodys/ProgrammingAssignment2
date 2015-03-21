## This is an assignment 2 solution for Course in Coursera: R Programming 
## Functions are to for the use of cach in order to reduce calculations of inverse
## matrices

## Function that defines a new class which stores the inverse matrix in memory instead of 
## recalculating it everytime

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(y) {
     x <<- y
     inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inverse <<- inverse
   getinverse <- function() inverse
   list(get = get, set = set, 
        getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve - returns the inverse matrix for the matrix object obtained by makeCacheMatrix
## in case the inverse matrix has been cached, the message is printed

cacheSolve <- function(x, ...) {
   inverse <- x$getinverse()
   if(!is.null(inverse)) {
     message('getting cached data')
     return(inverse)
   }
   inverse <- solve(x$get(), ...)
   x$setinverse(inverse)
   inverse        
}
