#' @title Plot legend for colored mesh.
#' @description Assuming a \code{mesh} is colored according to a vector of \code{values},
#'   the function takes the color sequence from the \code{mesh} and plots it
#'   associated to \code{values}.
#' @usage plotLegend(mesh,values,main)
#' @param mesh a \code{mesh3d} object
#' @param values a vector of continuous values associated to individual vertices
#'   of the \code{mesh}.
#' @param main plot title.
#' @return A plot of the color sequence associated to \code{values} on the \code{mesh}.
#' @export
#' @author Marina Melchionna, Silvia Castiglione
#' @importFrom graphics abline
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

plotLegend<-function(mesh,values,main){
  ind<-seq(min(values,na.rm=TRUE),max(values,na.rm=TRUE),length.out=200)
  nonavalues<-which(!is.na(values[order(values)]))
  indcol<-findInterval(ind,values[order(values)][nonavalues])
  colvec<-mesh$material$color[order(values)][nonavalues][indcol]
  plot(NA, main = main, xlab = "",ylab = "",xlim=range(values,na.rm = TRUE),ylim=c(0,1))
  abline(v = ind, col = colvec,lwd=20)
}
