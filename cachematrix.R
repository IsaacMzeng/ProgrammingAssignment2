## First function: 
##(1)set a function 'makeCacheMaxtrix' with 'x' a matrix as its argument.
##(2)'makeCacheMatrix' sets 'x' and caches it as 'y',
##(3)initialises 'inv' as an empty varible and caches is as 'NULL'(empty),
##(4)'get' carries 'x' 
##(5)'setInverse' carries 'matrixInverse' and caches 'matrixInverse' as 'inv'
##(6)'getInverse' carries 'inv'
##(7) returns a list of (set, get, setInverse, getInverse)

##Second function: 
##(1)sets function 'CacheSolve' with argumrnts x and others
##(2)extracts 'getInverse' from 'x' and passes it to inv 
##(3)if 'inv' is not equal to zero 'inv' is returned 
##(4)else inverse of 'x' is calculated and returned 'inv'
 

## firstvfunction creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    
    set <- function(y){
        
        x <<- y 
        
        inv <<- NULL
    }
    
    get <- function()x
    
    setInverse <- function(matrixInverse) inv <<- matrixInverse
    
    getInverse <- function()inv
    
    list(set = set, get = get, 
         
         setInverse = setInverse,
         
         getInverse = getInverse)
}


## second function that will calculate and return the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        
        message("get cached data")
        
        return(inv)
    }
    
    data <-x$get()
    
    inv <-solve(data, ...)
    
    x$setInverse(inv)
    
    inv
}
