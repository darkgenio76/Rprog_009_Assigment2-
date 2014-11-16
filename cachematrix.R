
## The makeCacheMatrix function is basically the same as the makeVector function, in that it is a list of 4 functions with the same goals   
## The argument changes to a matrix, the set function creates the special matrix 
## The get function is used to retrieve the matrix after the set function has created it
## The solvematrix function sets the value of m, which is the placeholder for the inverse matrix, which is assigned after the matrix has been inverted
## The getsolution function is used to retrieve the solution after it has been cached, and indeed it outputs null if its value has not been assigned by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solvematrix  <- function(cachedinverse) m <<- cachedinverse
  getsolution <- function() m
  list(set = set, get = get,
       solvematrix = solvematrix,
       getsolution = getsolution)
}

## The cacheSolve function checks first if the value of x$getsolution() has already been assigned.  
## If the output has already been assigned, it retrieves it without proceeding to the calculation.
## If m (the inverse of the matrix argument) is null, the function proceeds with the calculation using the solve() function
## Finally it stores the value of m in the "placeholder" or special matrix created in the makeCacheMatrix() function, and in this way it caches the output for future launches of the function

cacheSolve <- function(x, ...) {
  m <- x$getsolution()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$solvematrix(m)
  m
}
