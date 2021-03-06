## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Computes the inverse of the special matrix returned by the makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## Check to see if the inverse has already been calculated
        if(!is.null(inv)) {     
                message("getting cached data")
         ## Returns a matrix that is the inverse of 'x'
                return(inv)       
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}