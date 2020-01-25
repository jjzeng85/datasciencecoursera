## This programming provides us a method to cache data more efficiently from
## potentially time-consuming tasks like computing the inverse matrix of 
## a square matrix


## We first construct a function that creates a special vector that consists of
## some functions that set/get value of a matrix and set/get value of its
## inverse matrix, i.e. a vector that consists of "functions"

makeCacheMatrix <- function(X = matrix()) {
        v <- NULL
        set <- function(y){
                X <<- y
                v <<- NULL 
        }
        
        get <- function() X
        setinvr <- function(solve) v <<- solve
        getinvr <- function() v
        list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)
}


## The second function below calculates the inverse of the matrix from the special
## vector created with the above function
## It checks if the inverse has already been calculated. If so, get it! Otherwise
## calculate it and sets the value of the inverse in the cache via `setinvr` function


cacheSolve <- function(X, ...) {
        v <- X$getinvr()   
        if(!is.null(v)){
                message("getting cashed inverse matrix!")
                return(v)
        }
        matx <- X$get()
        v <- solve(matx)   ## Return a matrix that is the inverse of 'x'
        X$setinvr(v)
        v
}
