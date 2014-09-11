##The following functions cache the inverse of a matrix:


##Creates a matrix that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
	##Initialize the inverse
	i<-NULL
	
	##Setting the matrix
	set<-function(matrix){
		m<<-matrix
		i<<-Null
	}
	
	##Getting the matrix
	get<-function(){
		##Return the matrix
		m
	}
	##Setting the Inverse of the matrix
	setInverse<-function(inverse){
		i<<-inverse
	}
	##Getting the inverse
	getInverse<-function(){
		##Return inverse
		i
	}
	##Give a list of methods
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


##Obtaining inverse of matrix using "makeCacheMatrix"
##"cacheSolve" will get the inverse from the cache if the inverse has ##already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        
        ##Gives inverse if already set
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        ##Get matrix from object
        data<-x$get()
        
        ##Use multiplication to obtain inverse matrix
        m<-solve(data) %*% data
        
        ##Set inverse
        x$setInverse(m)
        
        ##Return matrix
        m
        
}

