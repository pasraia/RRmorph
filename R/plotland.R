#' @title Plot landmarks importance on 3d mesh
#' @description The function relates PCA loadings of a single PC axis to
#'   individual landmarks and plots them on a 3d mesh by means of interpolation.
#' @usage plotland(pca,sel=1,refsur=NULL,refmat=NULL,k=5,pal=NULL,
#'   defo=FALSE,radius=0.001)
#' @param pca the result of a relative warp analysis. Classes \code{relwarps} and
#'   \code{nosymproc} are supported.
#' @param sel numeric indicating the focal RW/PC axis.
#' @param refsur the \code{mesh3d} object to plot on. If \code{NULL}, the mesh
#'   is reconstructed by means of \code{\link[Rvcg]{vcgBallPivoting}} from the consensus
#'   configuration derived from \code{pca}.
#' @param refmat the landmark set related to \code{refsur}. If \code{NULL},
#'   the consensus configuration derived from \code{pca} is used.
#' @param pal a vector of colors to be passed to \code{\link[grDevices]{colorRampPalette}}.
#' @param k the argument \code{k} passed to \code{\link{interpolMesh}}.
#' @param defo when \code{refsur} and \code{refmat} are provided, \code{defo = TRUE}
#'   warps \code{refsur} on the consensus shape.
#' @param radius argument \code{radius} passed to \code{\link[rgl]{spheres3d}}
#' @return A list including a \code{mesh3d} object colored according to landmarks
#'   importance and a matrix of landmarks importance on each RW/PC axis. Additionally,
#'   the function returns a 3d plot of the mesh.
#' @export
#' @author Marina Melchionna, Silvia Castiglione, Carmela Serio, Giorgia Girardi
#' @importFrom grDevices colorRampPalette
#' @importFrom Morpho tps3d rotmesh.onto
#' @importFrom Rvcg vcgArea vcgBallPivoting
#' @importFrom rgl spheres3d
#' @examples
#'   \donttest{
#'   da<-"https://github.com/pasraia/RRmorph_example_data/raw/refs/heads/main/RRmorphdata.rda"
#'   download.file(url=da,destfile = paste0(tempdir(),"/RRmorphdata.rda"))
#'   load(paste0(tempdir(),"/RRmorphdata.rda"))
#'
#'   require(rgl)
#'   require(Morpho)
#'
#'   pca<-procSym(endo.set)
#'   ldm<-endo.set[,,"Homo_sapiens"]
#'   sur<-endo.sur[["Homo_sapiens"]]
#'
#'   plotland(pca=pca,sel=1,refsur = sur,refmat = ldm)
#'   }


plotland<-function(pca,sel=1,
                   refsur=NULL,refmat=NULL,
                   k=5,pal=NULL,defo=FALSE,radius=0.001){

  if(inherits(pca,"relwarps")){
    pca$mshape->mshape
    pca$bePCs->PCs
  }

  if(inherits(pca,"nosymproc")){
    pca$mshape->mshape
    pca$PCs->PCs
  }

  if((is.null(refmat)&!is.null(refsur))|(is.null(refsur)&!is.null(refmat))) {
    warning("One of refmat or refsur is missing: the surface will be automatically generated",immediate. = TRUE)
    refsur<-refmat<-NULL
  }

  totload<-loadmats<-list()
  for(i in 1:ncol(PCs)){
    loadmats[[i]]<-matrix(PCs[,i],ncol=3,nrow=length(PCs[,1])/3)
    totload[[i]]<-apply(loadmats[[i]],1,function(y){
      ppA <- (y %*% c(0,0,1))/unitV(y)
      ang<-rad2deg(acos(ppA))
      if(ang>90) unitV(y)*-1 else unitV(y)
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

  open3d()
  shade3d(mesh,specular="black")
  spheres3d(mat,radius=radius,col=mesh$material$color[id])
  plotLegend(mesh,values,main=NULL)

  return(list(mesh=temp_sur,totload=totload))
}
