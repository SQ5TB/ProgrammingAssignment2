## Function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##     - set the value of the matrix
##     - get the value of the matrix
##     - set the value of the inverse of a matrix (setsolve)
##     - get the value of the inverse of a matrix (getsolve)

makeCacheMatrix <- function(x = matrix()) {
    cs <- NULL
    set <- function(y) {
        x <<- y
        cs <<- NULL
    }
    get <- function() x
    setsolve <- function(s) cs <<- s
    getsolve <- function() cs
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function cacheSolve calculates iverse of a matrix which was created with the makeCacheMatrix function.
## It skips calculations and returns stored value if the value has been already been calculated.
## x param is makeCacheMatrix objcet

cacheSolve <- function(x, ...) {
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached inverse of matrix")
        return(s)
    }
    
    data <- x$get()
    
    if(dim(data)[1] != dim(data)[2]) {
        message("not a square matrix!")
        x$setsolve(NA)
        return(NA)
    }
    
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
