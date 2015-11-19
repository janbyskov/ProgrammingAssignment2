## The two functions makeCacheMatrix and cacheSolve in combination can hold
## a matrix and its inverse as well as calculate the inverse. 
## when the cached value is used, the message "getting cached data" is printed

## The use of the two functions is illustrated in the following example
##
## > cm <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(cm)
##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(cm)
## getting cached data
##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## This function defines a special datatype which can hold a 
## matrix as well as the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix in the special 
## Matrix datatype that can hold cached values
cacheSolve <- function(x, ...) {
        
        ## Get inverse
        i <- x$getinverse()
        
        ## If cache is present then return cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## otherwise calculate inverse matrix, cache and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
