## This pair of functions creates a special object (matrix) that stores a matrix and retrieves its cached inverse 
## when it finds that the inverse has already been calculated and the matrix has not changed

## This function creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This second function computes the inverse of the special "matrix" created by the function 'makeCacheMatrix' above. 
## When it finds that the inverse has already been calculated and the matrix has not changed, then it retrieves the
## inverse from the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if (!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
    }
