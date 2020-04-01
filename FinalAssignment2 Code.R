makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) i <<- inverse
  
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}






cachesolve <- function(x, ...) {
  
  i <- x$getinv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinv(i)
  
  i
}


test = function(mat){
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cachesolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cachesolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
}


mat1 = matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
test(mat1)

