#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=numeric()){
  m<-NULL
  set<-function(y=matrix()){
    matrix.maker<-function(nrow=sample(5:10,1),ncol=sample(5:10,1),data=sample((1:100),15120,replace=T))
    {matrix(data,nrow,ncol)}
    m<<-matrix.maker()
    x<<-y
  }
  # set <- function(y){ # 1.set the value of matrix
  #   x<<-y
  #   m<<-NULL
  # }
  
  get<-function()m #get the value of matrix
  setinverse<-function()a<<-solve(m) #set the value of inverse
  getinverse <-function()a # get the value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


aVector<-makeCacheMatrix()
aVector$set()
aVector$get()
aVector$setinverse()
aVector$getinverse()

#cacheSolve: This function computes the inverse of the 
#special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
  return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}

cacheSolve(aVector)