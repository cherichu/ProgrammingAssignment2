makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x, mat, ...) {
  data <- x$get()
  m <- x$getmatrix()
  if(!is.null(m) & all(data==mat)) {
    message("getting cached data")
    return(m)
  }
  x$set(mat)
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

