## cachematrix.R find the inverse of a matrix and if inverse is in cache then extract from there
## considering matrix passed is invertible

## makeCacheMatrix() storing the inverse in cache and having function for getting,setting the matrix
## and also getting and setting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    inv=NULL

    set<-function(mat,nr,nc){
        x<<-matrix(mat,nr,nc)
        inv<<-NULL
  
    }

    setinv<-function(p){
        inv<<-p
    }
      getinv<-function(){inv}
    get<-function(){x}
    
  
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)

}


## cacheSolve() function finds the inverse of the given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        if(!is.null(x$getinv())){
            # print("from cache")
            return (x$getinv())
        }
        
        inverse=solve(x$get())
        x$setinv(inverse)
        inverse
}
