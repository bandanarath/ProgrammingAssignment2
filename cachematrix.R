##The following functions are used to create a special object that stores a matrix and caches its inverse. 
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

## set the value of the matrix

## get the value of the matrix

## set the value of the inverse

## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
     
 }


## The function tries to check if the inverse available in cache else compute

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getinv()
     if(!is.null(inv)) {
         message("getting cached data")
	print ("getting cached data")
         return(inv)
     }
    message("no cached data,computing..")	
     data <- x$get()
        
     inv  <- solve(data, ...)
        
     x$setinv(inv)
        
     inv	 
}
