% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapeDiff.R
\name{shapeDiff}
\alias{shapeDiff}
\title{Shape difference between 3d meshes}
\usage{
shapeDiff(x,pca,refsur,refmat,mshape_sur = NULL,
  pal=NULL,NAcol="gray90",show.names=TRUE)
}
\arguments{
\item{x}{a vector of specimens pair.}

\item{pca}{the result of a relative warp analysis. Classes \code{relwarps} and
\code{nosymproc} are both accepted.}

\item{refsur}{a list of two \code{mesh3d} objects to be provided in the same order as \code{x}.}

\item{refmat}{a list of two landmark sets related to \code{refsur} to be
provided in the same order as \code{x}.}

\item{mshape_sur}{a \code{mesh3d} object used as a reference for mesh reconstruction.
The vertices of \code{mshape_sur} must be the consensus configuration. If \code{NULL},
it is automatically generated by applying \code{\link[Rvcg]{vcgBallPivoting}}
on the consensus configuration derived from \code{pca}.}

\item{pal}{a vector of colors to be passed to \code{\link[grDevices]{colorRampPalette}}.}

\item{NAcol}{the color associated to \code{refsur} vertices falling outside the
range of \code{refmat} (not involved in interpolation).}

\item{show.names}{logical: if \code{TRUE}, the names of the specimens as in \code{x}
are displayed in the 3d plot.}
}
\value{
Two \code{mesh3d} objects colored according to shape differences.
  Additionally, the function returns 3d plots of the meshes.
}
\description{
The function reconstructs two specimens' meshes (\code{x})
  by using their superimposed configurations (from within \code{pca}),
  calculates the shape difference between them, and plots such differences
  on provided meshes (\code{refsur}).
}
\examples{
  \donttest{
  da<-"https://github.com/pasraia/RRmorph_example_data/raw/refs/heads/main/RRmorphdata.rda"
  download.file(url=da,destfile = paste0(tempdir(),"/RRmorphdata.rda"))
  load(paste0(tempdir(),"/RRmorphdata.rda"))

  require(Morpho)

  pca<-procSym(endo.set)
  ldm_homo<-endo.set[,,"Homo_sapiens"]
  sur_homo<-endo.sur[["Homo_sapiens"]]
  ldm_macaca<-endo.set[,,"Macaca_fuscata"]
  sur_macaca<-endo.sur[["Macaca_fuscata"]]

  diffs<-RRmorph::shapeDiff(x=c("Homo_sapiens","Macaca_fuscata"),
                   pca = pca,refsur = list(sur_homo,sur_macaca),
                   refmat = list(ldm_homo,ldm_macaca))
  }
}
\author{
Marina Melchionna, Silvia Castiglione
}
