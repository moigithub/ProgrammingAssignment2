# makeCacheMatrix function will store the matrix (original) data and the inverse matrix
# and returns those data when requested
makeCacheMatrix <- function(x = matrix()) {
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

# cacheSolve function work togheter with makeCacheMatrix, 
# this will check first
# if inverse matrix was already solved and return
# else solve and store it on "cache" for posterior use
cacheSolve<- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
