## Functions that cache the inverse of a matrix
## This function creates a special matrix object that can cache its inverse

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv_matrix <- NULL
  set <- function(m){
    x <<- m
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv_matrix <<- solveMatrix
  getInverse <- function() inv_matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The function below computes the inverse of the special matrix returned by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_matrix <- x$getInverse()
  if(!is.null(inv_matrix))
    {
   
    message("getting cached data")
    return(inv_matrix)
    
  }
  
  data <- x$get()
  inv_matrix <- solve(data)
  x$setInverse(inv_matrix)
  inv_matrix      
  
}
