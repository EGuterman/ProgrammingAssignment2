## The function makeCacheMatrix creates a matrix 
## This matrix contains a list containing four functions
## The functions can then be called to return a cached matrix inverse

## Step 1: create matrix and set of functions which will
## set = create matrix according to whatever parameters given
## get = returns matrix that was created
## setinverse = cache matrix inverse within variable "i" in this function
## get inverse = returns matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function () x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve returns the inverse of the matrix being called
## The inverse is either retrieved from the cache if already computed
## Or, the inverse is computed and the answer cached by calling setinverse function above

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i))  {
               message("getting cached matrix inverse")
               return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
