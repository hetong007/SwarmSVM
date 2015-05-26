require(Matrix)

read.libsvm = function( filename, sparse = TRUE ) {
  content = readLines( filename )
  num_lines = length( content )
  space_ind = regexpr('\\s+',content)
  tomakemat = cbind(1:num_lines, -1, substr(content,1,space_ind-1))
  
  # loop over lines
  makemat = rbind(tomakemat,
                  do.call(rbind, 
                          lapply(1:num_lines, function(i){
                            # split by spaces, remove lines
                            line = as.vector( strsplit( content[i], ' ' )[[1]])
                            cbind(i, t(simplify2array(strsplit(line[-1],
                                                               ':'))))   
                          })))
  class(makemat) = "numeric"
  
  #browser()
  yx = sparseMatrix(i = makemat[,1], 
                    j = makemat[,2]+2, 
                    x = makemat[,3])
  if (!sparse)
    yx = as(yx,'matrix')
  return( yx )
}

rowl2norm = function(x) {
  rs = 1/sqrt(rowSums(x^2))
  new.x = Diagonal(x=rs) %*% x
  return(new.x)
}
