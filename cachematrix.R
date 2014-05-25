#The makeCacheMatrix function allows you to create a matrix,set the matrix's inverse, and to output the matrix and/or it's inverse
makeCacheMatrix <- function(mtx = numeric()){
  mtx_inverse <-NULL #This variable will hold the inverse of the matrix
  
  set <- function(y){
    mtx <<- y
    mtx_inverse <<- NULL
  }
  get <- function() mtx #This function returns the matrix
  setinverse <- function(inv)mtx_inverse <<- inv
  getinverse <- function() mtx_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#The cacheSolve function takes a matrix object that was created from the makecacheMatrix function as an argument
#The cacheSolve matrix calculates and returns an inverse matrix of the input matrix object. If the input matrix object already has an inverse then that value will be returned
cacheSolve <- function(mtx, ...){
  mtx_inverse <- mtx$getinverse()
  #Testing to see if the inverse already has been calculated
  if(!is.null(mtx_inverse)){
    message("getting cached inverse matrix")
    return(mtx_inverse)
  }
  
  data <- mtx$get()
  mtx_inverse <- solve(data)#calculating the inverse matrix
  x$setinverse(mtx_inverse)#setting the inverse matrix
  mtx_inverse#returning the inverse matrix
}