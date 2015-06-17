## The first fucntino will create a list taht contains the matrix and it's inverse

## The second function will check if a inverse is cached, if not calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  # Once the list is created, we can use the set function to change the matrix
  # In the same time, the cached inverse will be cleared
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  # Adding a if statement to make sure the matrix is a square matrix, else prompt a warning message
  if (nrow(data)==ncol(data)){
    i <- solve(data,...)
  }else{
    message("The Matrix need to be sqare matrix")
    return(invisible(i))
  }
  x$setinv(i)
  i
}
