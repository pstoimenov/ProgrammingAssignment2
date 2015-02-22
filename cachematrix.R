# The function makeCacheMatrix takes an invertible
# matrix as an argument and creates a list
# that contains 4 functions: set, get, setInv
# and getInv. It uses the <<- operator, so that
# the internal variables are not influenced by the
# outside environment. Note that the function "set" is not really
# needed, but I have included it to remain consistent
# with the original vector example.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }        
        get <- function() x 
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The function cacheSolve takes as an argument the output
# of the makeCacheMatrix function. It checks whether the 
# inverse has already been calculated in which case the
# message "getting cached data" is returned. If not the 
# inverse is calculated and the reult ist cached.

cacheSolve <- function(x, ...) {
        xinv <- x$getInv() 
        if(!is.null(xinv)) { 
                message("getting cached data")
                return(xinv) 
        }
        data <- x$get() 
        xinv <- solve(data) 
        x$setInv(xinv) 
        xinv 
}