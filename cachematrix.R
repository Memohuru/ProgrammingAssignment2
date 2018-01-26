## makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse
#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
#not changed), then the cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix<-function(x = matrix()){
  s<-NULL                  
  set <- function(y) {  
    x <<- y             # stores vector y as x in the containing enviroment (ce)
    s <<- NULL          # assigns a NULL value to s in the ce
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,    
       setsolve = setsolve,
       getsolve = getsolve)       # set the objet in a list 
  
}

cacheSolve <- function(x, ...) {
  s <- x$getsolve()   
  if(!is.null(s)) {
    message("getting cached data")
    return(s)   # if the matrix x has already been solved by cacheSolve (and consequently
    # stored in the ce, then we get the solved matrix and the message
  }
  data <- x$get()   # if the matrix x has not already been solved by cacheSolve, then 
  # the function() x of makeCacheMatrix runs and retrieves the x matrix
  
  s <- solve(data, ...)  # solve for the x matrix
  x$setsolve(s)          # set the solved matrix recently solved
  s                      # prints the recently solved matrix        
}


