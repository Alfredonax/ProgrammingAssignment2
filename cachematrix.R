## Calculate de inverse matrix 
## It uses the cache value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(mat){
        x <<- mat
        inv <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInv <- function(i){
        inv <<- i
    }
    
    getInv <- function(){
        inv
    }

    # By default return the list of inner functions
    list(set=set, get=get,setInv=setInv,getInv=getInv)
}


## Return an inverse of a matrix
## x must have been created with the wrapper makeCacheMatrix
cacheSolve <- function(x, ...) {

    inv <- x$getInv()

    if (!is.null(inv)){
        return (inv)
    }
    
    inv <- solve(x$get())
    x$setInv(inv)

    return(inv)
}
