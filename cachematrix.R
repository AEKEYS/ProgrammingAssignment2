## The functions below enable you to cache the inverse of a matrix.
## This can be computationally advantageous for resource-intensive calculations.

## The first function, makeCacheMatrix, creates a special "matrix" object (a list)
## that stores functions that can be used to operate on a matrix.

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL
    set <- function(y){
        x <<- y
        solution <<- NULL
    }
    get <- function() x
    setSolution <- function(inverse) solution <<- inverse
    getSolution <- function() solution
    list(set = set, get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}


## The second function, cacheSolve, computes or -- if not already calculated -- 
## retrieves the inverse of the matrix in the "makeCacheMatrix"-created object's environment.
## The function expects to be passed a list object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    solution <- x$getSolution()
    if(!is.null(solution)){
        message("getting cached data")
        return(solution)
    }
    message("calculating inverse")
    aMatrix <- x$get()
    solution <- solve(aMatrix, ...)
    x$setSolution(solution) # caching inverse
    solution
}
