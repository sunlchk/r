## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#1.  setting the value of the matrix
#2.  getting the value of the matrix
#3.  setting the value of the inverse
#4.  getting the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) m <<- Imatrix
  getImatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix`  
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

## At the moment giving: Error in x$getinv : $ operator is invalid for atomic vectors
cacheSolve <- function(x) {
  m <- x$getImatrix()
  if(!is.null(m)){
    message("Cached data found. Getting result... Done.")
    return(m)
  }
  else {
    message("No cached data found. Calculating inverse matrix...")
    data <- x$get() # obtains matrix from object x
    m <- solve(data) # finds inverse matrix
    x$setImatrix(m) # assigns resulting inverse matrix to object x
    message("Done.")
    return(m)
  }
}
