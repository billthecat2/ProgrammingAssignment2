## The functions in this assignment give an example of 
## caching time-consuming computations. The makeCacheMatrix
## function creates a matrix, calculates its inverse, and
## stores the inverse for future reference. The cacheSolve
## function retrieves the cached inverse if it exists, 
## otherwise, it invokes the makeCacheMatrix function's 
## setinverse subfunction.

## makeCacheMatrix takes as its arguments an integer vector,
## with the row and column count. This funciton assumes 
## the input matrix is invertible. That is, it is a suaare
## matrix and its determinant is not zero. makeCacheMatrix
## has four subfunctions and a variable:
## 1. m - variable that contains either NULL or the input
##    matrix's inverse.
## 1. set() - Creates a matrix using the arguments passed 
##   in from the makeCacheMatrix function. Running set()
##    will initialize the variable m to NULL.
## 2. get() - Returns the input matrix
## 3. setinverse() - Calculates the inverse of the input
##    matrix using the solve() function and saves the 
##    result to m stored in makeCacheMatrix.  
##    setinverse() will exit with an error if the matrix 
##    cannot be inverted.
##  4. getinverse() - Returns the input matrix's inverse.
##    If the inverse has not been calculated, returns NULL.

makeCacheMatrix <- function(x = numeric()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        
        getinverse <- function() m
        list(set = set, get = get, setinverse =setinverse,
             getinverse = getinverse)
        
        
}


## cacheSolve takes as its argument the matrix created by
## makeCacheMatrix() and checks the value of the makeCacheMatrix()
## variable m. If m contains the value of the inverse
##  (i.e. if m is not NULL), then m is returned. If m is NULL,
##  then cacheSolve invokes makeCacheMatrix$setinverse 
##  and returns the computed value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
