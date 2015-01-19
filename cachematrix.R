## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)!=nrow(x)) {
    print("Matrix is not square! Try again.")
  }
  else {
    ## m is NULL. 
    m <- NULL
    
    ## set is the function that determines initial matrix for the makeCacheMatrix "object".
    ## m assigned as NULL with every setting of a new matrix.
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## get is the function that returns initial matrix
    get <- function() x
    ## setinverse is the function that assigns parameter matrix to m
    setinverse <- function(solve) m <<- solve
    ## getinverse is the function that returns m value
    getinverse <- function() m
    
    ## Here is the list of inner functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  }
}


## Write a short comment describing this function

cacheSolve <- function(x,ini, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##If the initial matrix was changed before calling cacheSolve then a new value is assigned to 
  if (!(identical(ini,x$get()))) {
    message("Initial matrix has been changed...")
    x$set(ini)
  }
  m <- x$getinverse()
  
  ## If the function is called NOT for the first time then m is not null and we get the already saved data 
  if(!is.null(m)) {
    message("getting cached data")
    ##Returns m and quits the function running.
    return(m)
  }
  
  ## If the function is called for the first time then we get the initial matrix and call a solve function to it.
  data <- x$get()
  m <- solve(data, ...)
  ## and set result as inverse value back to the makeCacheMatrix object.
  x$setinverse(m)
  ## Print inverse matrix
  m
}
