## makeCacheMatrix makes an object that contains functions that get/set an invertible Matrix and its Inverse

makeCacheMatrix <- function(x = matrix()) {

  m<- NULL ## initialize inverse
  
  set <- function(y) {
    x <<- y 
    m<<- NULL
  }
  
  get <- function() x
  
  
  setInv <- function(invert) {
    m <<- invert 
    return m
  }
  
  getInv  <- function() m
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## If an inverted Matrix is found in cache, it is assigned, else Inverse is calculated through Solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

finalInv<- x$getInv() 
  

  if(!is.null(finalInv)) { 
    return(finalInv)
  }
  
  mat<- x$get()  
  
  finalInv <- solve(mat)
  
  x$setInv(finalInv)



}
