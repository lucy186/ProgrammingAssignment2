
## This function creates a matrix.
## It sets the value of the matrix, gets it and then sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ##Variable m is declared immediately
        m <- NULL

        set <- function(y) {
                ## set x to global scope
                x <<- y
                ## Update Value of Variable m
                ## Remember: Variable m is declared an initialised by makeCacheMatrix!
                m <<- NULL
        }
        get <- function() x
        ## set inverse of matrix to the global scope
        setinv <- function(inverse) m <<- inverse
        ## get inverse of matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
        

## makes inverse of matrix returned by makeCacheMatrix() if the inverse wasn't calculated before
## if value is calculated before CacheSolve() returns inverse matrix of makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get cached value
        m <- x$getinv()
        
        ## if inverse of matrix is cached before, return cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## else: get inverse of new input
        data <- x$get()
        ## assign data to m
        m <- inverse(data, ...)
        ## set the inverse of new matrix
        x$setinv(m)
        ## return the inverse of the matrix x
        m
}
