## The main goals of this functions are create, store and return a matrix and its inverse from the cache.

## This function creates the matrix

+makeCacheMatrix <- function(x = matrix()){    #Contructing and storing matrix  
  +  m <- NULL
  +  set <- function(y){
    +    x <<- y  
    +    m <<- NULL 
    +  }
  +  get <- function() 
    +  setInverse <- function(solve) m<<- solve #Setting inverse matrix
  +  getInverse <- function() m 
  +  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}



## This function call the matrix created in the previous function and calculates its inverse.

cacheSolve <- function(x, ...) {
  
  +  m <- x$getInverse()                
  +  if(!is.null(m)){                      #Checking the matrix for inverse matrix
    +    message("getting cached data")    
    +    return(m)                         #Printing inverse matrix  
    +  }
  +  data <- x$get()                       #Calling makeCacheMatrix function 
  +  m <- solve(data, ...)                 #calculating the inverse of the matrix
  +  x$setInverse(m)                       #storIng the inverse matrix in cache
}
