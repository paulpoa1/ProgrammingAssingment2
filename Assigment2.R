
## Write a short comment describing this function
## getMatrix to get matrix
## setMatrix to set matrix
## getInvMatrix to get inverse of matrix
## setInvMatrix to set inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  getMatrix <- function(){
    x
  }
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  getInvMatrix <- function(){
    invMatrix
  }
  
  setInvMatrix <- function(invMatrixParam){
    invMatrix <<- invMatrixParam
  }
  
  list(getMatrix = getMatrix,setMatrix = setMatrix,
       getInvMatrix = getInvMatrix,setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function
## Try to get inverse matrix if not null
## If NULL checks matrix is invertable using det function
## If matrix is not invertable returns NULL
## If matrix is invertable sets inverse matrix and return inverse matrix

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInvMatrix()
  
  if(!is.null(invMatrix)){
    message("Returning cached matrix")
    return(invMatrix)
  }
  
  data <- x$getMatrix()
  
  if(det(data) == 0){
    message("Non invertable matrix returning NULL")
    return(NULL)
  }
  
  invMatrix <- solve(data)
  x$setInvMatrix(invMatrix)
  invMatrix
}