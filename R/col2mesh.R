#' @title Color a mesh according to provided values
#' @description The function colors a \code{mesh} according to a vector of
#'   continuous \code{values} related to individual vertices.
#' @usage col2mesh(mesh,values,pal,from=NULL,to=NULL,NAcol="gray90")
#' @param mesh a \code{mesh3d} object.
#' @param values a vector of continuous values associated to individual vertices
#'   of the \code{mesh}.
#' @param pal a vector of colors to be passed to
#'   \code{\link[grDevices]{colorRampPalette}}.
#' @param from,to lower and upper \code{values} to be associated to the ends of
#'   \code{pal}.
#' @param NAcol the color associated to NA \code{values}.
#' @return A \code{mesh3d} object colored according to \code{values}.
#' @export
#' @author Marina Melchionna, Silvia Castiglione
#' @importFrom stats na.omit
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
#'   val<-rnorm(ncol(rec$vb))
#'
#'   interp<-interpolMesh(sur = rec,refsur = sur,refmat = ldm,
#'                        values = val,element ="vertices",k = 4)
#'
#'   colmesh<-col2mesh(mesh = sur,values = interp,pal = heat.colors(5))
#'   plotLegend(mesh = colmesh,values = interp, main = "Pan troglodytes")
#'   open3d()
#'   shade3d(colmesh,specular="black")
#'   }

col2mesh<-function(mesh,values,pal,from=NULL,to=NULL,NAcol="gray90") {

  if(!is.null(from)){
    if(any(na.omit(values)<from)) values[which(values<from)]<-NA
    cval<-c(from,values)

  } else cval<-values
  if(!is.null(to)){
    if(any(na.omit(cval)>to)) cval[which(cval>to)]<-NA
    cval<-c(cval,to)
  }

  l<-symdistr(cval)
  n=length(l$l)
  cols<-colorRampPalette(pal)(n)

  intervals <- seq(min(l$l,na.rm = TRUE), max(l$l,na.rm = TRUE), length.out = n)
  pos_cols<-findInterval(values,intervals)
  col <- cols[pos_cols]
  col[which(is.na(col))]<-"gray90"

  mesh$material$color<-col

  return(mesh)

}
