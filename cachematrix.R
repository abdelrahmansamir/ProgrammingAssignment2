## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix())
 {
    i <- NULL
    setMatrix <- function(y) 
   {
      ## caches the inputed matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)      
      x <<- y 
      #sets the value of i(the matrix inverse if used cacheSolve) to Null  
      i <<- NULL 
    }
    getMatrix <- function() x #gets the setted matrix
    setinv <- function(solve) i <<- solve #caching inverse(by using solve) of inputted matrix
    getinv <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix, setinv = setinv, getinv = getinv)
}


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{ 
    ## Return a matrix that is the inverse of 'x'
    i<- x$getinv()# if the inverse has already calculated this gets it.
    if(!is.null(i)) { # check to see if matrix already cached
      message("getting cached data")
      return(i) # returning cached matrix
    }
    data <- x$getMatrix() # getting input matrix values
    i<- solve(data, ...) # Applying inverse on that
    x$setinv(i) # Setting inverse
    i  # returning inverse of matrix
}
