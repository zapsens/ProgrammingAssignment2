## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## For this assignment, assume that the matrix supplied is always invertible.
##
## Example of use:
##  mymatrix<-matrix(rnorm(4),2,2)
##  mcm <- makeCacheMatrix(mymatrix)
##  res <- cacheSolve(mcm)
##  res2 <- cacheSolve(mcm)

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) mi <<- inv
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'.
## If the inverse has already been calculated (and the matrix has not changed), then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
