## Matrix inversion can be very costly. The objection of this
## functions is to cache the inverse of the matrix so that
## in a second moment it doesn't need to be recalculated

## makeCacheMatrix will have the getter and setters in order to
## cache the results

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list( set = set, get = get,
              setSolve = setSolve,
              getSolve = getSolve)
}


## Cache solve will inverse the matrix in case it hasn't been
## cached, otherwise it will use the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}



