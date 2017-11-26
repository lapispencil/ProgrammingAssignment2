## Programming Assignment 2 - Lexical Scoping

#The first function, makeCacheMatrix creates a special matrix, which is really a list 
#containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## The second function returns the inverse of the matrix. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and  skips the 
## computation. Otherwise, it calculates the inverse, sets the value in the cache via 
## the setinverse function.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("Getting cached data.")
      return(inv)
  }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Sample computation:
# > x <- matrix(10:7, nrow=2, ncol=2)
# > x
#       [,1] [,2]
# [1,]   10    8
# [2,]    9    7

# > mk <- makeCacheMatrix(x)

# > cacheSolve(mk)
#       [,1] [,2]
# [1,] -3.5    4
# [2,]  4.5   -5

# > cacheSolve(mk)

# Getting cached data.
#       [,1] [,2]
# [1,] -3.5    4
# [2,]  4.5   -5
 

