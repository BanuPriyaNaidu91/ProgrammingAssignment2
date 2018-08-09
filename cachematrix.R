## Caching the inverse of a Vector

makeCacheMatrix <- function(x = matrix()) {
## set the value of the vector
inv <- NULL
   set <- function(y) {
        x <<- y
         inv <<- NULL 
       }
    ## get the value of the vector
    get <- function() x 
   
   ##set the value of inverse
   setinverse <- function(inverse) inv <<- inverse
   
   ##get the value of inverse
   getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## calculating the inverse of the special "vector" created by function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    
    ## check if the inverse has already been calculated
    if(!is.null(inv)) {
        message("getting cached data.")
         return(inv)
     }
   
     ## If not calculated already, calculate the inverse of the data and set the value of the inverse in the cache via the setinverse function  
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}
