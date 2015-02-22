
# author:dogvile
# date:22/2/15

##################################################

#makeCacheMatrix function creates a list containing a function to
#with the setters and getters of both matrices (noraml,inverse) - some kind of get/set pattern



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


###################################################################



# The following function returns the inverse of the matrix. 
#checking whether is already an inverse matrix in order to avoid double calculation
# Otherwise, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


   