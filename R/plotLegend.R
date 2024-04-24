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
#'   interp<-interpolMesh(sur = rec_pan,refsur = sur_pan,refmat = ldm_pan,
#'                        values = val,element ="vertices",k = 4)
#'
#'   colmesh<-col2mesh(mesh = sur_pan,values = interp,pal = heat.colors(5))
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
