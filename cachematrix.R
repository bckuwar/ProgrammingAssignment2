## Function makeCacheMatrix is to cache a cpu intensive "matrix inverse" operation.  Creates a special "vector", which is really 
## a list containing a functions to: 1. set the matrix, 2. get the matrix, 3. set the inverse and 4. get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                        ## set the value of matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                         ## get the value of matrix
    
    setinverse <- function(inv) m <<- inv       ## cache matrix inverse
    
    getinverse <- function() m                  ## get matrix inverse from cache
    
    list(set = set, get = get,                  ## return list of functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve functions calculates the inverse the first time it's called.  It then stores the results in cache so that
## subsequent calls will return results directly from cache.  
 
cacheSolve <- function(x, ...) {
    m <- x$getinverse()                         ## get matrix inverse from cache
    if(!is.null(m)) {                           ## if not null, then return results from cache
        message("getting cached data")
        return(m)
    }
    data <- x$get()                             ## get matrix
    m <- solve(data, ...)                       ## get inverse *cpu intensive*
    x$setinverse(m)                             ## set cache
    m						## Return a matrix that is the inverse of 'x'
}
