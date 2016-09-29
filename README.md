# Cachematrix#
# Week 3 Assignment#

makeCacheMatrix<-funciton(x = matrix()){
        inv<-NULL
makeCacheMatrix<-function(x = matrix()){
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinversion<-function(inversion) inv<<-inversion
        getinversion<-function() inv
        list(set = set,get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


cacheSolve<-function(x,...){
        inv<-x$getinversion()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinversion(inv)
        inv
}
