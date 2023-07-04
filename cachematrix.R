## Setup the ability to cache an object (Matrix)
## Setup the getters and setters

makeCacheMatrix <- function(x = matrix(sample(1:50,9),2,2)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Using appropriate scope, and call the solve function
## Computing inverse of a square matrix

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inversed matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}