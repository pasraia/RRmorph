#' @title Interpolate values on a 3d mesh
#' @description The function takes a reconstructed \code{mesh3d} object
#'   (\code{sur}) with some related \code{values} (to either triangles or
#'   vertices of the mesh) and transfers such values to the real mesh
#'   (\code{refsur}) from which \code{sur} was derived.
#' @usage
#' interpolMesh(sur,values,refsur,refmat,element=c("triangles","vertices"),k=4)
#' @param sur a reconstructed \code{mesh3d} object with vertices matching to
#'   \code{refmat}.
#' @param values the vector of values related to \code{sur} to be interpolated.
#'   \code{values} can be related to either triangles or vertices (see
#'   \code{element}).
#' @param refsur the reference mesh (\code{mesh3d} object) to interpolate the
#'   \code{values} on.
#' @param refmat the landmark set related to \code{refsur}.
#' @param element one of \code{"triangles"} or \code{"vertices"}, depending on
#'   which of them \code{values} is related to.
#' @param k the number of nearest neighbor vertices used for interpolation (see
#'   details).
#' @return The vector of values related to each vertex of \code{refsur}.
#' @details The function starts by locating a set of points (NNps) on
#'   \code{refsur}, each being the single nearest neighbor for each vertex of
#'   \code{sur} (or barycenter if \code{element="triangles"}). Then,
#'   interpolation is performed by identifying the \code{k} points among NNps
#'   being the closest to each vertex of \code{refsur} and computing the mean of
#'   their \code{values} weighted by their distance.
#' @export
#' @author Marina Melchionna, Silvia Castiglione
#' @importFrom Morpho mcNNindex vert2points barycenter
#' @importFrom stats dist weighted.mean
#' @examples
#'   \donttest{
#'   da<-"https://github.com/pasraia/RRmorph_example_data/raw/refs/heads/main/RRmorphdata.rda"
#'   download.file(url=da,destfile = paste0(tempdir(),"/RRmorphdata.rda"))
#'   load(paste0(tempdir(),"/RRmorphdata.rda"))
#'
#'   require(rgl)
#'   require(Morpho)
#'   require(Rvcg)
#'
#'   pca<-procSym(endo.set)
#'   ldm<-endo.set[,,"Homo_sapiens"]
#'   sur<-endo.sur[["Homo_sapiens"]]
#'
#'   rec<- vcgBallPivoting(pca$mshape, radius = 0)
#'   rec$vb[1:3,]<-t(ldm)
#'   val1<-rnorm(ncol(rec$vb))
#'
#'   # Interpolate values associated to vertices
#'   val1<-rnorm(ncol(rec$vb))
#'   interp1<-interpolMesh(sur = rec,refsur = sur,refmat = ldm,
#'                         values = val1,element ="vertices",k = 4)
#'
#'   colmesh1<-col2mesh(mesh = sur,values = interp1,pal = heat.colors(5))
#'   open3d()
#'   shade3d(colmesh1,specular="black")
#'
#'
#'   # Interpolate values associated to triangles
#'   val2<-rnorm(ncol(rec$it))
#'   interp2<-interpolMesh(sur = rec,refsur = sur,refmat = ldm,
#'                         values = val2,element ="triangles",k = 4)
#'
#'   colmesh2<-col2mesh(mesh = sur,values = interp2,pal = heat.colors(5))
#'   open3d()
#'   shade3d(colmesh2,specular="black")
#'   }
interpolMesh<-function(sur,values,refsur,refmat,
                       element=c("triangles","vertices"),k=4){

  refpoints<-vert2points(refsur)
  idref<-mcNNindex(refsur,refmat,k=1)[,1]
  if(element=="triangles"){
    df<-data.frame(c(1:nrow(refmat)),idref)

    temp_mesh<-sur
    temp_mesh$vb<-t(cbind(refpoints[df[,2],],1))

    bary<-barycenter(temp_mesh)
    idbary<-mcNNindex(refsur,bary,k=1)[,1]

  }else idbary<-idref

  clostInd <- mcNNindex(refpoints[idbary,],refsur, k = k)
  distInd <- apply(clostInd,2,function(i) sqrt(rowSums((refpoints-refpoints[idbary[i], ])^2)))

  interd<-t(apply(refmat,1,function(j) {
    a<-mcNNindex(refmat,t(j),k = 2)
    apply(refmat[a[-1],,drop=FALSE],1,function(x) dist(matrix(c(j,x),nrow = 2,byrow = TRUE)))
  }))

  nowhiteid<-which(!apply(distInd,1,mean)>max(interd))

  allvalues<-rep(NA,nrow(clostInd))
  allvalues[nowhiteid]<-sapply((1:nrow(clostInd))[nowhiteid], function(i)
    weighted.mean(values[clostInd[i,]],(1-range01(distInd[i,]))))

  return(allvalues)
}
