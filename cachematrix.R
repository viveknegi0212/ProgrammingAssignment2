
##Our aim in this experiment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i<<- inverse
        getinv <- function() i
        
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv)

}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- m$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data) %*% data
        x$setmean(i)
        i
}
