#' @title Triangles to vertices
#' @description The function transfers values associated to triangles of a
#'   \code{mesh3d} object to its vertices.
#' @usage tri2verts(mesh,values)
#' @param mesh a \code{mesh3d} object.
#' @param values a vector of continuous values associated to individual triangles
#'   of the \code{mesh}.
#' @return A vector of continuous values associated to individual vertices of
#'   the \code{mesh}.
#' @export
#' @author Marina Melchionna, Silvia Castiglione
#' @examples
#'   \donttest{
#'   da<-"https://github.com/pasraia/RRmorph_example_data/raw/refs/heads/main/RRmorphdata.rda"
#'   download.file(url=da,destfile = paste0(tempdir(),"/RRmorphdata.rda"))
#'   load(paste0(tempdir(),"/RRmorphdata.rda"))
#'
#'   require(Morpho)
#'   require(Rvcg)
#'
#'   pca<-procSym(endo.set)
#'   ldm<-endo.set[,,"Homo_sapiens"]
#'   sur<-endo.sur[["Homo_sapiens"]]
#'
#'   rec<- vcgBallPivoting(pca$mshape, radius = 0)
#'   rec$vb[1:3,]<-t(ldm)
#'   val<-rnorm(ncol(rec$vb))
#'
#'   vertval<-tri2verts(rec,val)
#'   }


tri2verts<-function(mesh,values){
  bary<-barycenter(mesh)
  refpoints<-vert2points(mesh)
  clostInd <- sapply(1:nrow(refpoints),function(y) which(apply(mesh$it,2,function(z) any(z==y))))
  clostInd[which(sapply(clostInd,length)==0)]<-NA

  allvalues <- sapply(1:length(clostInd), function(i) {
    if (all(is.na(clostInd[[i]]))) NA else{
      distInd<-sqrt(rowSums((refpoints[i,]-bary[clostInd[[i]],,drop=FALSE ])^2))
      weighted.mean(values[clostInd[[i]]], (1 - range01(distInd)))
    }
  })

  return(allvalues)
}
