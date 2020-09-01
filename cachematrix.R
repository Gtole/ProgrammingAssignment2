## Put comments here that give an overall description of what your
## functions do

## once the matrix is created, I call the function with the matrix and compute the inverse
A<-matrix(c(0,1,0,2,1,1,2,1,2),3,3)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m <<- NULL
  }
  
  get<-function()x
  
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function(inverse) m
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## recover the inverse from cache and check if is not null

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## And finally print the inverse

A1<-makeCacheMatrix(A)

cachesolve(A1)


