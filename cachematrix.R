## PMatrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly
## Below are two functions that cache the inverse of a matrix with a case study.

##Set Value of vector, get value of vector, set value of inverse, get value of inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Calculates inverse of above "special" vector
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##Setting following variables submits a simple square matrix, returns the inverse
##and multiples the original matrix by the inverse matrix to find identity martrix.
##TestMatrix<-matrix(1:4,2,2)
##myMatrix<-makeCacheMatrix(TestMatrix)
##cacheSolve(myMatrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##TestMatrix
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##TestMatrix%*%cacheSolve(myMatrix)
##getting cached data
##      [,1] [,2]
##[1,]    1    0
##[2,]    0    1