## take a sent Matrix.  determine its inverse and cache the inverse Matrix. 

makecachedMatrix <- function(sentMatrix = matrix()) 
{
  cachedMatrix <- NULL
  
  set <- function(mySet) 
  {
    sentMatrix <<- externalMatrix
    cachedMatrix <<- NULL
  }
  
  get <- function() cachedMatrix
  setinverse <- function(inverse) cachedMatrix <<- externalMatrix
  getinverse <- function() cachedMatrix
  
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## take a matrix. if inverse is cached, return it.  
##  if cache is dirty, refresh it before you retrun it.   
  
cacheSolve <- function(sentMatrix, ...) 
{
  ## Return a matrix that is the inverse of the setnMatrix
  cachedMatrix <- sentMatrix$getinverse
  
  if (!is.null(cachedMatrix)) 
  {
    return(cachedMatrix)
  }
  
  myMatrix <- sentMatrix$get()
  cachedMatrix <- solve(myMatrix, ...)
  sentMatrix <- setInverse(cachedMatrix)
  
  cachedMatrix
  
}
