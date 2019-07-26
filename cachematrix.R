## R programming - caching the inverse of a  matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The following functions are used to create a special object that stores a matrix and caches its inverse. The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

##keep:set the value of the matrix

##receive: get the value of the matrix

##keepinverse:set the value of the inverse

##receiveinverse: get the value of the inverse


makeCacheMatrix <- function(y = matrix()) {
  a <- NULL
  keep <- function(x) {
    y <<- x
    a <<- NULL
  }
  receive <- function() y
  keepinverse <- function(inverse) a <<- inverse
  receiveinverse <- function() a
  list(keep = keep,
       receive = receive,
       keepinverse = keepinverse,
       receiveinverse = receiveinverse)
}

##ASSUMPTION- matrix is invertible
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(y, ...) {
  a <- y$receiveinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- y$receive()
  a <- solve(data, ...)
  y$keepinverse(a)
  a
}

  
## testing the function
## We test the function by creating a 4 by 4 matrix. we assign it letter z.
  

z <- matrix(c(20,10,40,15,22,31,24,35,11,23,54,66,55,43,28,11),4,4)
j <- makeCacheMatrix(z)
cacheSolve(j)## inverse after entire computations.

##Hence we are able to create a cache of inverse matrix
