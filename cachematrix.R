## I have created two functions makeCacheMatrix() and cacheSolve().
## 


## makeCacheMatrix will be used to assign to parent variables and returns a list. 

makeCacheMatrix <- function(myMatrix = matrix()) {
        invMat <- NULL                      # Initializing the inverse matrix
        set <- function(y) {                      
                myMatrix <<- y
                invMat <<- NULL
        }
        get <- function() myMatrix
        setInvMat <- function(matInverse) invMat <<- matInverse
        getInvMat <- function() invMat
        list(set = set, get = get, setInvMat = setInvMat,getInvMat = getInvMat)
}

## Computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse of the matrix was already calculated then value gets retrived from Cache.

cacheSolve <- function(x, ...) {
        invMat <- x$getInvMat()
        if(!is.null(invMat)) {
                message("getting cached inverse matrix")
                return(invMat)
        }
        dataMatrix <- x$get()
        invMat <- solve(dataMatrix, ...)
        x$setInvMat(invMat)
        invMat
}
## Implementation

x <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
mat<-makeCacheMatrix(x)
cacheSolve(mat) ## uncached solution
cacheSolve(mat) ## cached solution as it was already calculated before.

