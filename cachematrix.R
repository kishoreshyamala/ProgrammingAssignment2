makeCacheMatrix <- function(x) {
        invm<-NULL
        set<-function(new_matrix){
                x<<-new_matrix
                invm<<-NULL
        }
        get<-function() x
        setinv<-function(solve) invm<<- solve
        getcache<-function() invm
        list(set=set, get=get, setinv=setinv, getcache=getcache)
}

cacheSolve <- function(x) {
        invm<-x$getcache()
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        matrix<-x$get()
        invm<-solve(matrix)
        x$setinv(invm)
        invm
}