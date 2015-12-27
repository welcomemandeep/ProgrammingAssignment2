## The two programs should be used in conjunction to get inverse of a matrix
## First invoke makeCacheMatrix with a matrix to make matrix available as free
## varibles
## matrix2_2<-matrix(c(1,1,-1,2),  nrow=2, ncol=2)
## storeMakeCacheMatrix <- makeCacheMatrix(matrix2_2)
## Now use cacheSolve to get the inverse of the matrix
## cacheSolve(storeMakeCacheMatrix)
## If you invoke cacheSolve second time, it will fetch from cache



## Returns a list of methods of gettters and setters as follows
##	getter - Gets the matrix
## 	setter - Sets the matrix
## 	getInverse - Returns the cached inverse of the matrix
## 	setInverse - Sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	## initialize inv to NULL
	inv <- NULL
	setter <- function(y) {
	
		x <<- y
		inv <<- NULL
	}
	getter <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
## the function retuns the list of getters and settes
        list(setter = setter, getter = getter,
             setInverse = setInverse,
             getInverse = getInverse)
	

}


## The method returns inverse of the matrix. It uses solve method so 
## expects the given matrix to be intertible 
## @param - It expects a list of getter, setters of the matrix,  
## getter and setter of inverse of matrix
##
cacheSolve <- function(x, ...) {
## 	Check if the value exist in cache
	inv <- x$getInverse()
	if(!is.null(inv)) {
     	message("getting cached inverse of the supplied matrix")
		return(inv)
	}
## Oops, inverse is not cached, lets cache it
## 	get matrix
	data_matrix <- x$getter()
## Use solve method to get inverse
	inv<- solve(data_matrix, ...)
	x$setInverse(inv)
## Return a matrix that is the inverse of 'x'
	inv
}
