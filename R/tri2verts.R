#' @title Triangles to vertices
#' @description The function transfers values associated to triangles of a
#'   \code{mesh3d} object to its vertices.
#' @usage tri2verts(mesh,values)
#' @param mesh a \code{mesh3d} object
#' @param values a vector of continuous values associated to individual triangles
#'   of the \code{mesh}.
#' @return A vector of continuous values associated to individual vertices of
#'   the \code{mesh}.
#' @export
#' @author Marina Melchionna, Silvia Castiglione
#' @examples
#'   \dontrun{
#'   require(rgl)
#'   data(DataSimians)
#'   pca<-DataSimians$pca
#'   ldm_pan<-DataSimians$ldm_pan
#'   sur_pan<-DataSimians$sur_pan
#'
#'   rec_pan<- Rvcg::vcgBallPivoting(pca$mshape, radius = 0)
#'   rec_pan$vb[1:3,]<-t(ldm_pan)
#'   val<-rnorm(ncol(rec_pan$vb))
#'
#'   vertval<-tri2verts(rec_pan,val)
#'   }


tri2verts<-function(mesh,values){
  bary<-barycenter(mesh)
  refpoints<-vert2points(mesh)
  clostInd <- sapply(1:nrow(refpoints),function(y) which(apply(mesh$it,2,function(z) any(z==y))))
  clostInd[which(sapply(clostInd,length)==0)]<-NA

  allvalues <- sapply(1:length(clostInd), function(i) {
    if (all(is.na(clostInd[[i]]))) NA else{
      distInd<-sqrt(rowSums((refpoints[i,]-bary[clostInd[[i]],,drop=FALSE ])^2))
      weighted.mean(values[clostInd[[i]]], (1 - RRphylo:::range01(distInd)))
    }
  })

  return(allvalues)
}
