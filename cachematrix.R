## Put comments here that give an overall description of what your



# the makeCacheMatix function  creates a special "matrix" that caches its inverse. 
#It creates a list that contains 4 functions: set, get, setmatrix and getmatrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL #this is where the result of inversion is stored and is initialized to null
    # In the following function we set matrix (x) to  be substituted by (y) in the main function makeCacheMatrix 
    # It also restores the value of the inverse to null
    set<-function(y){ 
        x<<-y
        inv<<-NULL
    }
    ##The functions setmatrix and getmatrix store the value of the input in a variable inv into the main function makeCacheMatrix 
    #(setmatrix) sets the value and (getmatrix) returns this value .

    get<-function() x
    setmatrix<-function(solve) inv<<- solve
    getmatrix<-function() inv
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}  
}

#The cacheSolve function verifies whether the value inv exists or is null using the getmatrix function. It has been stored in the previous function
#if it already exists, it returns the value of inv and a message.
#if it doesn't exist then the function calculates the inverse using solve function 

cacheSolve <- function(x, ...) {
        inv<-x$getmatrix()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        matrix<-x$get()
        inv<-solve(matrix, ...)
        x$setmatrix(inv)
        inv
    }
    
