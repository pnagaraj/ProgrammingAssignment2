## makecachematrix returns a list of functions set,get,setinverse
## getinverse and cachesolve returns the inverse of the matrix 

## functions to set, get matrix 
## if matrix inverse does not exist, then it is computed
## if matrix inverse exists, then it is retrieved 

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get<-function()x
    setinv <- function(solve) ix <<- solve
    getinv <- function() ix
    list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Has functions to compute the inverse of a matrix if one does not
## exist or to retrieve it if it was computed before

cacheSolve <- function(x, ...) {
    ix <- x$getinv()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data)
    x$setinv(ix)
    
        ## Return a matrix that is the inverse of 'x'
    ix
}
## source cachematrix.R
## mc<-makeCacheMatrix(),cs<-cacheSolve(mc)
