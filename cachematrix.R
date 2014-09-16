## The following functions will cache the inverse of a matrix.


## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setMatrix <- function(solve) {
                i <<- solve
        }
        
        getMatrix <- function() {
                i
        }
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. In summary, cachesolve function will return a matrix
## that is the inverser of 'x'.

cacheSolve <- function(x, ...) {
        
        i <- x$getMatrix()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        else {
                data <- x$get()
                i <- solve(data, ...)
                x$setMatrix(i)
                i
        }
}