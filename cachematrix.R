######################################################################################
## Matrix inversion is usually a costly computation and there may be some 		##
## benefit to caching the inverse of a matrix rather than compute it repeatedly 	##
## This is a pair of functions that cache the inverse of a matrix.			##
######################################################################################

################################################################################
## This function creates a special "matrix" object that can cache its inverse ##
################################################################################
makeCacheMatrix <- function(mat = matrix()) {
	
	pInvMat <- NULL

	#set the value of the matrix
	set <- function(m) {
		mat <<- m
            pInvMat <<- NULL
       }
       
	#get the value of the matrix
	get <- function() mat

	#set the value of the inverse matrix
      setInvMat <- function(invMat) pInvMat <<- invMat
      
	#get the value of the inverse matrix
	getInvMat <- function() pInvMat
      
	list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat )
}


################################################################################
## This function computes the inverse of the special "matrix" returned by 	##
## makeCacheMatrix above. If the inverse has already been calculated (and the	## 
## matrix has not changed), then the cachesolve should retrieve the inverse 	##
## from the cache.										##
################################################################################
cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	invMat <- x$getInvMat()

      if(!is.null(invMat)) {
      	message("getting cached data")
            return(invMat)
	}
	
	mat <- x$get()
      invMat <- solve(mat)

      x$setInvMat(invMat)
      invMat
}
