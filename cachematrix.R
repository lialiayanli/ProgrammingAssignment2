

# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix
# 4 get the value of the inverse of the matrix


# use either solve() or inverse() to calculate the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
 m<- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

# The following first check if the inverse of this matrix has been computed, 
# if so then get cached data;
# if not then compute before giving the data


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Test Run
x = rbind(c(1, 32), c(32, 1))
x
m = makeCacheMatrix(x)
#first run
cacheSolve(m)
#get the cached results
cacheSolve(m)

