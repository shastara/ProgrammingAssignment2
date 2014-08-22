#to determine whether inverse has already been calculated, if yes will get the inverse from the cache, if not will calculate the inverse

#creates matrix object to cache inverse
makeCacheMatrix <- function(x=numeric()) {
	#setting value of the matrix
	m <- NULL 
	set <- function(y) {
		x<<-y
		m<<-NULL
	}
	#getting value of the matrix
	get<-function() x
	#setting value of the inverse
	setmatrix<- function(ginv) m <<-ginv
	#getting value of the inverse
	getmatrix<- function() m
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
}

#computes inverse of matrix
cacheInverse <- function(x, ...) {
	m<-x$getmatrix
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-ginv(data, ...)
	x$setmatrix(m)
	m
}