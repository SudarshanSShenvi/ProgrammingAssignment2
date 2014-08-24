#######################################################################
#  Peer Assessments /Programming Assignment 2: Lexical Scoping
#######################################################################
#  function makeCacheMatrix
#  makeCacheMatrix creates a special "matrix" object to cache its inverse

			makeCacheMatrix <- function(A = matrix()) {
					# Initialize I (inverse) to NULL
						I <- NULL
					
					# function setMat - Sets value of the matrix
						setMat <- function(matVar) {  
									A <<- matVar
									I <<- NULL  	        
									}
									
					# function getMat - Gets value of the matrix
					getMat <- function() A   
					
					# function setMatInv - Sets value of inverse to I
					setMatInv <- function(matInvVar) I <<- matInvVar 
					
					# function getMatInv - Gets value of I
					getMatInv <- function() I 
					
					# functions embedded in the list
					list(setMat = setMat, getMat = getMat, setMatInv = setMatInv,	getMatInv = getMatInv) 
				}

#  function cacheSolve
#  cacheSolve computes the inverse of the matrix created with the makeCacheMatrix function. 

			cacheSolve <- function(A, ...) {
			# get the cache 
			I <- A$getMatInv() 
			
			# Check if inverse calculated and available
			if(!is.null(I)) { 
			message("get cache")
			return(I)
			}
			
			# Get matrix
			data <- A$getMat() 
			
			# Compute inverse
			I <- solve(data, ...) 
			
			# Save inverse value in the cache
			A$setMatInv(I) 
			
			# return the inverse
			I 
			}