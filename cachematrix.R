## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #inverse is assigned null
  set <- function(y) {   #the function to set the matrix
    x <<- y              # the matrix y is chached in variable x
    inv <<- NULL
  }
  
  get <- function() x    #returns the matrix
  inv <<- solve(x)       #cache the inverse of the matrix
  setinverse <- function(solve) inv  #sets the inverse of the matrix
  getinverse <- function() inv       #returns the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()               #retrieves the inverse of the matrix x
  if(!is.null(inv)) {                 #tests whether the inverse is null
    message("getting cached data")    
    return(inv)                       #returns the inverse
  }
  data <- x$get()                     #retrieves the matrix
  inv <- solve(data, ...)         #finds the inverse if it has not been found already
  x$setinverse(inv)                   #stores the inverse
  inv                                 #returns the inverse
  
}
