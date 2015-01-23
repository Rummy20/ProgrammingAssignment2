##  This code helps compute the inverse of a matrix. Once computed the value is
## cached and retrieved for any future reference. This reduces the computation time
## and speeds up the program


## makeCacheMatrix accepts a matrix input
## returns a list of 3 functions - get, getInv, setInv
##  get returns the matrix (sounds convoluted but is needed)
## getInv returns the cached inverse values
## setInv stores the inverse in cache when computed the very first time

makeCacheMatrix <- function(x = matrix()) {
  
  invMatCache<-NULL
  
  get <- function () x
  
  setInv<- function (x) {invMatCache<<-x}  # caching the inverse of matrix
  
  getInv<- function() invMatCache
  
  list(get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve accepts a list with 3 functions (get,getInv,setInv) and returns
## inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat<-x$getInv()
  
  if(!is.null(invMat)){
    message("Getting Cached matrix")
    return(invMat)  
  }# So the cached value is returned 
  
  #Inverse needs to be computed because invMat is null
  data<-x$get()
  invMat<-solve(data)
  x$setInv(invMat)
  invMat
  
}
