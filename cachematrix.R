makeCacheMatrix<- function(mat = matrix()){
  inv<- NULL
  
  set <- function (matrix){
    mat<<- Matrix
    inv<<- NULL
      
  }
  
  get<- function(){
    mat
  }
  
  setInverse<- function(inverse){
    inv<<-inverse
  }
  getInverse<- function(){
    inv
  }
  
  #list of all methods
  list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
  
}

cacheSolve<- function(m,...){
  
  
  inv<- m$getInverse()
  
  if(!is.null(inv)){
        message("getting cached data")
        return (inv)
      }
  data<- m$get()
  inv<- solve(data, ...)
  m$setInverse(inv)
  inv
  
}

