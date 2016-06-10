## The combination of these functions are meant to find the inverse of a 
## matrix-argument and cache the respective results in temporary memory 
## in order to avoid repeatedly calculating the inverse of the matrix


## The makeCacheMatrix() function takes a matrix-argument and creates a list
## of function arguments which all serve to cache the results of the matrix's inverse
## which is determined by another function call from cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    invmatrixobj<-NULL
    stage<-function(y){
        x<<-y
        invmatrixobj<<-NULL
    }
    getarg<-function(){
        x
    }
    setmatrix<-function(argsolve){
        invmatrixobj<<-argsolve#gets its argument from cachesolve call to solve(ans)
    } 
    getinvmatrix<-function(){
        invmatrixobj#this object is being sourced from the output from the setmatrix() 
        #function as a result of the golbal assignment
    }
    list(stage=stage,getarg=getarg,setmatrix=setmatrix,getinvmatrix=getinvmatrix)#creates a list of functions
    #which can be called. Note the list names and the function names must match
}


## The cacheSolve() funtion assigns a matrix and checks to see if the object is avaiable or not. 
## If the object is available it returns the cached results. If the matrix object is not, it 
## calculates the inverse of the matrix provided in the makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrixobj<-x$getinvmatrix()#calls the getinvmatrix() function within the makeCacheMatrix() function
    if(!is.null(invmatrixobj)){#checks to see if anything is cached/available already
        message("pulling cached data")
        return(invmatrixobj)
    }
    message("calculating...")
    ans<-x$getarg()#calls the getarg() function within the makeCacheMatrix() function
    invmatrixobj<-solve(ans)
    x$setmatrix(invmatrixobj)#calls the setmatrix() function within the makeCacheMatrix() function
    invmatrixobj
}
