## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 1- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #    Variable 'Inverse' will store the inverse of the matrix, by default it is set to null
  Inverse <- NULL                                  
   # Calculation of inverse of matrix and storing it in cache  namely 'Inverse' 
  
  #1-set the value of the Matrix
  # Set function will store the value of Matrix 
  set <- function(y) {
    
    x <<- y      # updating the old matrix 'X' to the new matrix 'Y'                           
    Inverse <<- NULL  #  since the value of matrix is updated so flush the cache
  }
  
  
  #2-get the value of the Matrix
  get <- function() 
    {
    x
  }
  
  
  #3-set the value of the inverse of matrix
  setinverse <- function(solve) 
  {  
    Inverse <<- solve  
    }
  
  #4-get the value of the inverse of matrix
  getinverse <- function()
    {
    Inverse  
  }
  
  # This list with the available functions
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Store the inverse of the matrix
  Inverse <- x$getinverse()        
  
  # checking the cache if the inverse of the matrix has been calculated
  if(!is.null(Inverse)) {  
    # if its already present in cache then print the message "Getting Cached Data"
    message("Getting Cached Data")  
    # returns the cached inverse of the matrix and stops further the computation.
    return(Inverse)                       
  }
  #If the inverse has not been calculated:
  
  # get the matrix  
  GetMatrix <- x$get()  
  
  # using solve calculate the inverse of the matrix
  Inverse <- solve(GetMatrix, ...)       
  
  # updating the cache value
  x$setinverse(Inverse)   
  
  # output the calculated value of matrix
  Inverse                                       
}
