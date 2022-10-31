## makeCacheMatrix creates a special "matrix" that can set the value of the
## matrix, which is really a list containing a function to:
## set the value of the matrix
##get the value of the matrix 
##set the value of the inverse  
##get the value of the inverse.

## As a result of this function, you can cache the inverse of the matrix 
## created by this function.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                } 
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set = set, 
                     get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        
## By calling this function,the inverse of the special "matrix" returned 
## by makeCacheMatrix will be computed. As long as the matrix does not change, 
## cacheSolve should retrieve the inverse if it has already been calculated.

cacheSolve <- function(x,...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

X <- matrix(c(1,1,2,3),2,2)
Xm <- makeCacheMatrix(X)
cacheSolve(Xm)
## [,1] [,2]
## [1,]    3   -2
## [2,]   -1    1
cacheSolve(Xm)
## getting cached data
## [,1] [,2]
## [1,]    3   -2
## [2,]   -1    1
