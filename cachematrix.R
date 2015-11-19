##This function will calculate the inverse of the matrix x and will save the result and when we need this
##information again, we can take it from where the information is saved

## makeCacheMatrix will save the matrix x, and the function solve

makeCacheMatrix<-function(x=matrix()){
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setmean<-function(solve) s<<-solve
        getmean<-function() s
        list(set=set, get=get, setmean=setmean, getmean=getmean)
}



## This function will calculate the invers of X if we didn't calculate it before, if we did it,
##this function will take the value we calculated.

cacheSolve<-function(x,...){
        s<-x$getmean()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setmean(s)
        s
}