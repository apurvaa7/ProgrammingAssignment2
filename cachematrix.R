## These 2 functions create a matrix inverse
## and cache the result. After the function is called for the first time, the result is retrieved from the cache for subsequent calls

## Computes the inverse of the input matrix and caches the result

makeCacheMatrix <- function(x = matrix()) {
Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of the input from the cache if available else
## computes the result and sets the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv

}

## Example:
##  > B = matrix(
## + c(2,4,3,1),
## + nrow=2,
## + ncol=2)
## > m = makeCacheMatrix(B)
## > cacheSolve(m)
##   [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
## > cacheSolve(m)
## getting cached data
##    [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2


