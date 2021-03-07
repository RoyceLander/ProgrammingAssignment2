## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function to generate the matrix. It will then
## set the inverse of the matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
        matrix1 <- NULL
        set <- function(y) {
                x <<- y
                matrix1 <<- NULL
        }
        get = function() x
        setinv = function(inverse) matrix1 <<- inverse
        getinv = function() matrix1
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve will attempt to return the cached inverse of x. If it has
## not been calculated, it will then do the calculation.

cacheSolve <- function(x, ...) {

        matrix1 = x$getinv()
        if(!is.null(matrix1)) {
                message("getting cached data")
                return(matrix1)
        }
        data <- x$get()
        matrix1 = solve(data, ...)

        x$setinv(matrix1)

        return(matrix1)
}
m0 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m0)
cacheSolve(myMatrix_object)
