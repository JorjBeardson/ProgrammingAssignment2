## Put comments here that give an overall description of what your
## functions do

## creates a list containing four elements, get, set, getsoln and setsoln
## I is the value I've chosen to hold the inverse as m was for the original function
## "set" sets the global variable x to the value it's passed and the global variable I to NULL
## "get" passes on the data
## "setsoln" gets passed the solution then sets the global variable I to the value it's passed
## "getsoln" returns the solution as defined in setsoln thanks to the joys of lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setsoln <- function(solve) I <<- solve
  getsoln <- function() I
  list(set=set, get=get, setsoln=setsoln, getsoln=getsoln)
}


## cacheSolve gets passed an output from makeCacheMatrix and checks to see if an inverse has been created
## if so, it returns the value in "getsoln" from makeCacheMatrix
## if not, load the data from "get" in makeCacheMatrix
## then set local variable I to the output from "solve" (the inverse of the matrix)
## pass the inverse to "setsoln" so it can be cached

cacheSolve <- function(x, ...) {
  I <- x$getsoln()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setsoln(I)
  I
}
