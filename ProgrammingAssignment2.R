# simpler way to calulate matrix inverse using functions and global environment


## create special "matrix" object that caches inverse
makeCacheMatrix <- function(mat = matrix()) {
    
    inv <- NULL # initialize inverse
    
    # set matrix in global env.
    set <- function(x) {
        mat <<- x
        inv <<- NULL
    }
    
    get <- function() mat # fn. to get matrix
    setInv <- function(inverse) inv <<- solve(mat) # fn. uses solve to set value
    getInv <- function() inv # gets inverse
    
    # create list of functions to return
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## computes inverse of "special" matrix object
cacheSolve <- function(mat, ...) {
    
    inv <- mat$getInv() # get cached matrix
    
    # if value exists return it
    if (!is.null(inv)) {
        message("Getting cached matrix")
        return(inv)
    }
    
    # if no inverse, get matrix stored in object
    dat <- mat$get() 
    inv <- solve(dat,...)# matrix multiplication for inverse
   
    mat$setInv(inv) # set inverse to object
    inv
}





