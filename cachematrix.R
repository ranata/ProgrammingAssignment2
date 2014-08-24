## Cachematrix script contains two functions that demonstrate how Lexical Scoping rules 
## can be used to calculate the inverse of matrix and cache the result 
## by preserving state inside of an R Object

## makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to:

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Setter method to set the matrix in cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Getter method to retrieve the matrix from cache
  get <- function() x
  
  ## Setter method to set the inverse of the matrix in cache
  setinverse <- function(inverse) inv <<- inverse

  ## Getter method to get the inverse of the matrix from cache
    getinverse <- function() inv
  
  
  ## return a list containing all the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function calculates the inverse of the special "matrix" created with makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
  ## Fetch inverse matrix for x

  inv <- x$getinverse()
  
  ## Check if the inverse exists in cache and return the inverse if it exists
  
  if(!is.null(inv)) {
    print("Retrieving inverse from cache")
    return(inv)
  }
  
  data <- x$get()
  
  ## check if x has a valid inverse; if doesn't, return NULL
  if (det(data) == 0) {
    print("Inverse does not exist")
    return(NULL)
      
  }
  ## calculate and return the inverse after setting it in cache
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

## Following lines provide sample data and commands for running the script

## x <- matrix(c(1, 4, 5, 6, 2, 8, 9, 4, 2), nrow=3, ncol= 3)
## a<-makeCacheMatrix()
## a$set(x)
## cacheSolve(a)  // first time calculates and returns the inverse
## cacheSolve(a)  // second time returns the inverse from cache


