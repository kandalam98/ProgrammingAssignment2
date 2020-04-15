## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
    i<-NULL
    
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    i<<-NULL
    
  }
  ## Method the get the matrix
  get<-function()
  ## Return the matrix
  x
  
  ## Method to set the inverse of the matrix
  setinverse<-function(inverse)
  i<<-inverse
  
  ## Method to get the inverse of the matrix
  getinverse<-function()
   ## Return the inverse property
  i
  
  ## Return a list of the methods
  list(set = set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  
  ## Just return the inverse if its already set
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from our object
  data<-x$get()
  
  ## Calculate the inverse using matrix multiplication
  i<-solve(data,...)
  
  ## Set the inverse to the object
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
       
}
