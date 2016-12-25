
## return: a alist that contains functions
##       -- set the matrix
##       -- get the matrix
##       -- set the inverse
##       -- get the inverse
##      will use this as input to cachesolve()
## x is a  square of invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        ## <<- used to assign value to an object in environment
        # different from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse = function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## x: output of makeCacheMatrix()

cacheSolve <- function(x, ...) {
                
    ## Return inverse of the original matrix input to makeCacheMatric()
    inv = x$getinverse()
    ##in inverse has already been calculated
    if(!is.null(inv)){
        ##get it from cache and skip further computation
        message("getting cached data")
        return(inv)
    }
    
    #otherwise calculate inverse
    data = x$get()
    inv = solve(data)
    
    ##set the value of inverse in cache through setinv function
    x$setinverse(inv)
    return(inv)
}


## Test run
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## mk = makeCacheMatrix(x)

## > mk$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## > # No cache hence creating one
## > cacheSolve(mk)
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
 
## > ##Retrieve from cache
## > cacheSolve(mk)
## getting cached data
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
