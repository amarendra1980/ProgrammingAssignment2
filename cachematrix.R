## Set the values of a matrix or accept it as an input
## get the values of matrix and its inverse from cache
## If matrix and its inverse are not present in cache then calculate it

## This function will set the values of a matrix, get the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i<- NULL
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



## This function will get the inverse from cache 
## If not in cache, it will caculate the inverse and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
