
## we will be accepting a matrix, then inversing it and caching it


#cacheSolve(makeCacheMatrix(matrix(c(1:4),2,2)))

## numeric() creates a real vector of a specified length,
## the elments of this vector are all equal to 0
## by default 
#########################################################

makeCacheMatrix <- function(x = matrix()) {
                 i <- NULL
                 set <- function(y) {
                 x <<- y
                 i <<- NULL
                     }
       get <- function() x  
       setinverse <- function(solve) i <<- solve
       getinverse <- function() i
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## we will be checking if inverse exists yet and if it does pull that
## else if it doesnt then we will comput the inverse. 

cacheSolve <- function(x, ...) {
           i <- x$getinverse()
          if(!is.null(i)) {
            message("getting cached data")
             return(i)
              }
           data <- x$get()
           i <- solve(data, ...)
           x$setinverse(i)
           i
}
