## makeCacheMatrix: This function creates a special object "matrix"
## that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set= set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}
## cacheSolve: This function calculates the inverse of the "matrix" special returned
## by makeCacheMatrix above. If the reverse is already calculated (and the matrix
## has not changed), then the inverse cachesolve to retrieve the cache.
cacheSolve <- function(x, ...) {
i <- x$getinverse()
if (!is.null(i)){
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
