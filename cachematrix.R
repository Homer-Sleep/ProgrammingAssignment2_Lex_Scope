
# makeCacheMatrix() function creates an environment and organizes the input matrix data 
# so that cacheSolve() can figure the inverse of the matrix with solve(). Additionally, it stores the 
# output of cacheSolve(), allowing cacheSolve() to retrieve it instead of computing it again. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # clear from previous vector data. 
  set <- function(y) {
    x <<- y # <<- makes a variable within parent environment 
    m <<- NULL # clears m again, good to have as part of set()
  }
  get <- function() x # this function takes advantage of the lexical scoping features in R. 
  # Since the symbol x is not defined within get(), R retrieves it from the parent 
  # environment 
  setsolve <- function(input) m <<- input #This function puts 'm' in the makeCacheMatrix() environment. 
  getsolve <- function() m # simply returns 'm'
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Return a matrix that is the inverse of matrix environment function defined by makeCacheMatrix()

cacheSolve <- function(x, ...) {
       
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
