#' @import Matrix
#' @importClassesFrom SparseM matrix.csr
#' @import LiblineaR
#' @import e1071
#' @import stats
#' @import kernlab kkmeans

read.libsvm = function( filename, sparse = TRUE, dims = NULL) {
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
  
  if (!is.null(dims)) {
    yx = sparseMatrix(i = makemat[,1], 
                      j = makemat[,2]+2, 
                      x = makemat[,3],
                      dims = dims)
  } else {
    yx = sparseMatrix(i = makemat[,1], 
                      j = makemat[,2]+2, 
                      x = makemat[,3])
  }
  
  if (!sparse)
    yx = as(yx,'matrix')
  return( yx )
}

#' svmguide1
#' 
#' An astroparticle application from Jan Conrad of Uppsala University, Sweden. 
#' 
#' @docType data
#' @keywords datasets
#' @name svmguide1
#' @usage \code{data(svmguide1)}
#' @format Two data objects \code{svmguide1} and \code{svmguide1.t}. 
#'    The first column is the target variable.
#' 
NULL

#' usps
#' 
#' J. J. Hull. A database for handwritten text recognition research. 
#'   IEEE Transactions on Pattern Analysis and Machine Intelligence, 16(5):550-554, May 1994. 
#' 
#' @docType data
#' @keywords datasets
#' @name usps
#' @usage \code{data(usps)}
#' @format Two data objects \code{usps} and \code{usps}. 
#'   The first column is the target variable.
#' 
NULL

#' ijcnn1
#' 
#' Danil Prokhorov. IJCNN 2001 neural network competition.
#'   Slide presentation in IJCNN'01, Ford Research Laboratory, 2001. 
#' 
#' @docType data
#' @keywords datasets
#' @name ijcnn1
#' @usage \code{data(ijcnn1)}
#' @format Two data objects \code{ijcnn1} and \code{ijcnn1}. 
#'   The first column is the target variable.
#' 
NULL

#' mnist
#' 
#' Yann LeCun, L. Bottou, Y. Bengio, and P. Haffner. Gradient-based learning applied to document recognition.
#'   Proceedings of the IEEE, 86(11):2278-2324, November 1998. 
#' 
#' @docType data
#' @keywords datasets
#' @name mnist
#' @usage \code{data(mnist)}
#' @format Six data objects \code{mnist38}, \code{mnist38.t}, \code{mnist49}, \code{mnist49.t}, 
#'   \code{mnistoe} and \code{mnistoe.t}. 
#'   The first column is the target variable.
#' 
NULL
