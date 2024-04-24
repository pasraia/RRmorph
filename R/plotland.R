#' @title Plot landmarks importance on 3d mesh
#' @description The function relates PCA loadings of a single PC axis to
#'   individual landmarks and plots them on a 3d mesh by means of interpolation.
#' @usage plotland(relw,sel=1,refsur=NULL,refmat=NULL,k=5,pal=NULL,
#'   defo=FALSE,radius=0.001)
#' @param relw the result of a relative warp analysis. Classes \code{relwarps} and
#'   \code{nosymproc} are supported.
#' @param sel numeric indicating the focal PC axis.
#' @param refsur the \code{mesh3d} object to plot on. If \code{NULL}, the mesh
#'   is reconstructed by means of \code{\link[Rvcg]{vcgBallPivoting}} from the consensus
#'   configuration derived from \code{relw}.
#' @param refmat the landmark set related to \code{refsur}. If \code{NULL},
#'   the consensus configuration derived from \code{relw} is used.
#' @param pal a vector of color to be passed to \code{\link[grDevices]{colorRampPalette}}.
#' @param k the argument \code{k} passed to \code{\link{interpolMesh}}.
#' @param defo when \code{refsur} and \code{refmat} are provided, \code{defo = TRUE}
#'   warps \code{refsur} on the consensus shape.
#' @param radius argument \code{radius} passed to \code{\link[rgl]{spheres3d}}
#' @return A list including a \code{mesh3d} object colored according to landmarks
#'   importance and a matrix of landmarks importance on each PC axis. Additionally,
#'   the function returns a 3d plot of the mesh.
#' @export
#' @author Marina Melchionna, Silvia Castiglione, Carmela Serio, Giorgia Girardi
#' @importFrom grDevices colorRampPalette
#' @importFrom Morpho tps3d rotmesh.onto
#' @importFrom Rvcg vcgArea vcgBallPivoting
#' @examples
#'   \dontrun{
#'   require(rgl)
#'   data(DataSimians)
#'   pca<-DataSimians$pca
#'   ldm_pan<-DataSimians$ldm_pan
#'   sur_pan<-DataSimians$sur_pan
#'
#'   plotland(pca,sel=1,refsur = sur_pan,refmat = ldm_pan)
#'   }


plotland<-function(relw,sel=1,
                   refsur=NULL,refmat=NULL,
                   k=5,pal=NULL,defo=FALSE,radius=0.001){

  if(inherits(relw,"relwarps")){
    relw$mshape->mshape
    relw$bePCs->PCs
  }

  if(inherits(relw,"nosymproc")){
    relw$mshape->mshape
    relw$PCs->PCs
  }

  if((is.null(refmat)&!is.null(refsur))|(is.null(refsur)&!is.null(refmat))) {
    warning("One of refmat or refsur is missing: the surface will be automatically generated",immediate. = TRUE)
    refsur<-refmat<-NULL
  }

  totload<-loadmats<-list()
  for(i in 1:ncol(PCs)){
    loadmats[[i]]<-matrix(PCs[,i],ncol=3,nrow=length(PCs[,1])/3)
    totload[[i]]<-apply(loadmats[[i]],1,function(y){
      ppA <- (y %*% c(0,0,1))/RRphylo:::unitV(y)
      ang<-RRphylo:::rad2deg(acos(ppA))
      if(ang>90) RRphylo:::unitV(y)*-1 else RRphylo:::unitV(y)
    })
  }

  totload<-do.call(cbind,totload)
  colnames(totload)<-colnames(PCs)

  if(is.null(pal)) pal<-c("#0a1045","#00c2d1","white","gold2","orangered")
  # colmap_tot <- colorRampPalette(pal)

  if(is.null(refsur)) {
    mat<-mshape
    temp_sur<-vcgBallPivoting(mshape, radius = 0)
  } else {
    if(defo) temp_sur<-tps3d(refsur,refmat,tarmat = mshape) else {
      temp_sur<-rotmesh.onto(refsur,refmat,mshape,scale = TRUE)$mesh
      mat<-rotmesh.onto(refsur,refmat,mshape,scale = TRUE)$yrot
    }
  }

  values<-interpolMesh(temp_sur,totload[,sel],temp_sur,mat,element = "vertices",k=k)
  mesh<-col2mesh(temp_sur,values,pal = pal)
  id<-mcNNindex(temp_sur,mat,k = 1)[,1]

  rgl::open3d()
  rgl::shade3d(mesh,specular="black")
  rgl::spheres3d(mat,radius=radius,col=mesh$material$color[id])
  plotLegend(mesh,values,main=NULL)

  return(list(mesh=temp_sur,totload=totload))
}
