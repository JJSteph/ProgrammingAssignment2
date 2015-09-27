## R Programming Coursera
## Week 3 programming assignment
## Github ID: JJSteph
## 9/26/15


## The makeCacheMatrix function creates a list that contains for different functions. 
## These four functions will be used in the cacheSolve function.
##
## Here are the four functions:
##
## get - This returns the value x
## set - This changes the vector x if needed
## setinv - This stores the inv in object m
## getinv - This gets the value stored in object m

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve function uses the output from makeCacheMatrix and returns the
##   inverse of the input matrix.
##
## If the inverse of the input matrix was previously calculated,
##   it will return a cached version.
##
## Otherwise it will calculate and return the inverse of the input matrix.


cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}



## Here is an example of how to use the two functions:

#x <- rbind(c(1,2) , c(3,4))
#x
#solve(x)


#z <- makeCacheMatrix(x)
#cacheSolve(z)


## This also works:

#cacheSolve(makeCacheMatrix(x))

