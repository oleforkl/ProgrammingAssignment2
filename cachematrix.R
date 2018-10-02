
## These two functions take a matrix as an input and turns it into the
## inverse matrix and store this inverse matrix into its cache until
## a new matrix is given.

## The function makeCacheMatrix takes the matrix x as an argument and
## comes out with a list of four functions that will be used by
## the cacheSolve function. By its own the makeCacheMatrix is not very
## usefull. It can only set x and return x.

makeCacheMatrix <- function(x = matrix()) {
        im<-NULL
        set<-function(y){
                x<<-y
                im<<-NULL
        }
        get<-function() x
        setimatrix<-function(imatrix) im <<- imatrix
        getimatrix<-function() im
        list(set = set, get = get, setimatrix = setimatrix,
             getimatrix = getimatrix)
}


## The cacheSolve function takes the makeCacheMatrix as argument.
## It checkes if the getimatrix() allready has produced the inverse
## matrix of x. If the inverse matrix already exist it will return 
## this as cached data. If the inverse matrix does not exist then it
## will calculate it and return this.

cacheSolve <- function(x, ...) {
       im<-x$getimatrix()
       if(!is.null(im)){
               message("getting cached data")
               return(im)
       }
       data<-x$get()
       im<-solve(data, ...)
       x$setimatrix(im)
       im
}
