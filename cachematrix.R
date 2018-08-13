## Program to compute inverse of matrix for Johns Hopkins Data Science Specialization
## R Programming, Week 3, Assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL}
    get<-function() x
    setsolve<-function(solvedmatrix) m<<-solvedmatrix
    getsolve<-function() m
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m<-x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)}
    data<-x$get()
    m<-solve(data,...)
    x$setsolve(m)
    m
}
