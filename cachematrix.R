## Together the two functions below allow user to calculate and cache the inverse of a 
## given matrix so that the inverse is only calculated once and can be returned from
## the cache in the future

## This function creates a special "matrix" object that can cache its inverse.
## So that in the future, if the inverse is requested, it can be called from cache
## Assumption: an invertible square (nxn) matrix is always passed in
makeCacheMatrix <- function(x = matrix()) {

     ##inititalize inv
     inv <- NULL
     
     ##set will set x to the value it receives and then resets inv
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     ##get returns what was passed in
     get <- function() x
     
     ##setinv gives inv the value of what it's passed
     setinv <- function(solve) inv <<- solve
     
     ##getinv prints what is stored in inv
     getinv <- function() inv
     
     #create a list for output
     list(set=set,get = get, setinv = setinv, getinv = getinv)
}

##This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
     ##Check whether the inverse has already been calculated and cached
     ##If so tell user it is in cache, print solution and end this function
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("Getting cached data")
          return(inv)
     }
     
     ##If inverse has not previously been calculated
     ##Get the matrix and calculate the inverse using the solve function in R
     data <-x$get()
     inv <- solve(data, ...)
     
     ##Cache the inverse for future use
     x$setinv(inv)
     
     ## Return a matrix that is the inverse of 'x'
     inv
        
}
