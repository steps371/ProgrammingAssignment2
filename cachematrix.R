## These functions are collectively used to calculate and store the inverse of a matrix

## The first function is a "list" of funtions to:
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      set_matrix <- function(matrix) m <<- matrix
      get_matrix <- function() m
      list(set = set, get = get,
           set_matrix = set_matrix,
           get_matrix = get_matrix )
      
}


## The second function gets the value of the matrix,
## checks whether an inverse of the input matrix has already been calculated and cached
## If it has, then the previous cached value will be returned
## Otherwise, the inverse matrix is calculated, stored and displayed

cacheSolve <- function(x = matrix, ...) {
         m <- x$get_matrix()
         if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$set_matrix(m)
         m
          }
        ## Return a matrix that is the inverse of 'x'
