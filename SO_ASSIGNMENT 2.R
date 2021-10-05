## So, Stephanie Mae 
## MATH 144

SO <- function(a = matrix()) {
  
  karugtong <- NULL 
  tama <- function(b){
    a <<- b
    karugtong <<- NULL
  }
  G <- function() {a}
  SI <- function(inverse) {karugtong <<- inverse}
  GI <- function() { karugtong }
  list(tama=tama, G=G, SI=SI, GI=GI)
}

SOLVE_CACHE_SO <- function(a, ... ) {
  
  karugtong <-  a$GI()
  
  if (!is.null (karugtong)) {
    message("invalid.")
    message("cacheing data.")
    
    return(karugtong)
  }
  
  synonym <- a$G()
  karugtong <- solve(synonym, ...)
  
  a$SI(karugtong)
  karugtong
}

