##      Computing the Inverse of a Matrix is usually a costly computation
##      and there may be some benefit to caching the inverse of a matrix rather
##      than computing it repeatedly.

##      ASSUMPTION :  It is assumed that the matrix supplied is always
##      invertible square matrix.If o/w "solve" function will return an error.


##      The first function, makeCacheMatrix creates a special "matrix",
##      which is really a list containing a function to

##      set the value of the matrix
##      get the value of the matrix
##      set the value of the Inverse of the matrix
##      get the value of the Inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function (y){
        x <<-  y
        i <<-  NULL
    }
    
    get <- function()x
    setsolve <- function(inverse)  i <<- inverse
    getsolve <-function ()i
    
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

#       The following function returns the inverse of the matrix. It first checks if
#       the inverse has already been computed. If so, it gets the result and skips the
#       computation. If not, it computes the inverse, sets the value in the cache via
#       setinverse function.


cacheSolve <- function(x, ...) {
    
    i <- x$getsolve()
    
    if(!is.null(i)){
        message("Getting cached data.") #***************
        return(i)
    }
    
    data<- x$get()
    i<- solve(data)
    x$setsolve(i)
    i
}

## Sample Run :

# > x <- matrix(sample(1:20,9),3,3)
# > x

# [,1] [,2] [,3]
# [1,]    5   10    3
# [2,]   13    4    1
# [3,]   12   18   11

# > y <- makeCacheMatrix(x)
# FIRST RUN for y
# > cacheSolve(y)
# [,1]        [,2]         [,3]
# [1,] -0.04180064  0.09003215  0.003215434
# [2,]  0.21061093 -0.03054662 -0.054662379
# [3,] -0.29903537 -0.04823151  0.176848875

## SECOND RUN for the same y
# > cacheSolve(y)
# Getting cached data.*****************************
# [,1]        [,2]         [,3]
# [1,] -0.04180064  0.09003215  0.003215434
# [2,]  0.21061093 -0.03054662 -0.054662379
# [3,] -0.29903537 -0.04823151  0.176848875


