## ----include = FALSE----------------------------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
     !rmarkdown::pandoc_available()) {
   warning(call. = FALSE, "Pandoc not found, the vignettes is not built")
   knitr::knit_exit()
}

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
options(rgl.useNULL = TRUE)
rgl::setupKnitr(autoprint = TRUE)

options(rmarkdown.html_vignette.check_title = FALSE,mc.cores=2,rgl.useNULL=TRUE)
load("vignette_data.rda")
library(RRmorph)
library(Morpho)
library(Rvcg)
library(rgl)


## ----loaddata, eval=FALSE-----------------------------------------------------
#  library(RRmorph)
#  library(Morpho)
#  library(Rvcg)
#  library(rgl)
#  library(Arothron)
#  
#  load("RRmorphdata.rda")
#  

## ----pca, eval=FALSE----------------------------------------------------------
#  pca_endo<-procSym(endo.set)
#  pca_cran<-procSym(crania.set)
#  
#  plot(pca_endo$PCscores[,1:2],pch=16,col=as.factor(dat.prima$group),
#       main = "PC1&2 plot - endocasts",asp = 1)
#  text(pca_endo$PCscores[,1:2],labels = dat.prima$species,pos = 3,cex=0.6)
#  plot(pca_cran$PCscores[,1:2],pch=16,col=as.factor(dat.prima$group),
#       main = "PC1&2 plot - crania",asp=1)
#  text(pca_cran$PCscores[,1:2],labels = dat.prima$species,pos = 3,cex=0.6)

## ----pcplotfig,out.width='95%',dpi=300,echo=FALSE,fig.align='center'----------
knitr::include_graphics("pcplot.png")

## ----ratemapfig,out.width='95%',dpi=300,echo=FALSE----------------------------
knitr::include_graphics("ratemap.png")

## ----ballpivoting,fig.align="center",fig.dim=c(7,4)---------------------------
mshapeE<-vcgBallPivoting(pca_endo$mshape)
mshapeC<-vcgBallPivoting(pca_cran$mshape)

mfrow3d(nr=1,nc=2,sharedMouse=TRUE)
shade3d(mshapeE,col="lightgray",specular="black")
wire3d(mshapeE,col="black",specular="black")
next3d()
shade3d(mshapeC,col="lightgray",specular="black")
wire3d(mshapeC,col="black",specular="black")

## ----rrphylo, eval=FALSE------------------------------------------------------
#  PCscore_endo<-RRphylo::treedataMatch(tree.prima,pca_endo$PCscores)$y
#  RRendo<-RRphylo::RRphylo(tree.prima,PCscore_endo)
#  
#  PCscore_cran<-RRphylo::treedataMatch(tree.prima,pca_cran$PCscores)$y
#  RRcran<-RRphylo::RRphylo(tree.prima,PCscore_cran)
#  
#  rm_endo<-rate.map(x = c("Macaca_fuscata","Homo_sapiens"),
#                    RR = RRendo,
#                    scores = PCscore_endo,
#                    pcs = pca_endo$PCs,
#                    mshape = pca_endo$mshape)
#  

## ----rm_endo,fig.align="center",fig.dim=c(7,4),echo=FALSE---------------------
mfrow3d(nr=1,nc=2,sharedMouse=TRUE)
shade3d(rm1sur[[1]],specular="black")
title3d(names(rm1sur)[1])
next3d()
shade3d(rm1sur[[2]],specular="black")
title3d(names(rm1sur)[2])

## ----rate.map.plot, eval=FALSE------------------------------------------------
#  endo.list<-arraytolist(endo.set[,,c("Macaca_fuscata","Homo_sapiens")])
#  
#  rm_endoS<-rate.map(x = c("Macaca_fuscata","Homo_sapiens"),
#                     RR = RRendo,
#                     scores = PCscore_endo,
#                     pcs = pca_endo$PCs,
#                     mshape = pca_endo$mshape,
#                     refsur = endo.sur,
#                     refmat = endo.list)
#  

## ----rm_endoS,fig.align="center",fig.dim=c(7,4),echo=FALSE--------------------
mfrow3d(nr=1,nc=2,sharedMouse=TRUE)
shade3d(rm2sur[[1]],specular="black")
title3d(names(rm2sur)[1])
next3d()
shade3d(rm2sur[[2]],specular="black")
title3d(names(rm2sur)[2])

## ----search.conv, eval=FALSE--------------------------------------------------
#  SC<-search.conv(RR = RRcran, y = PCscore_cran)
#  # Please note this result is not significant because we cut the data to reduce their size

## ----conv.map, eval=FALSE-----------------------------------------------------
#  cran.list<-arraytolist(crania.set[,,c("Alouatta_guariba",
#                                      "Alouatta_pigra",
#                                      "Hapalemur_griseus",
#                                      "Propithecus_verreauxi")])
#  
#  cm_crann<-conv.map(x1=c("Alouatta_guariba","Alouatta_pigra"),
#                     x2=c("Hapalemur_griseus","Propithecus_verreauxi"),
#                     scores = PCscore_cran,pcs = pca_cran$PCs,
#                     mshape = pca_cran$mshape,refmat = cran.list,refsur = crania.sur)
#  

## ----fig5,out.width='75%',echo=FALSE,fig.align="center"-----------------------
knitr::include_graphics("convmap1.png")

## ----angle.comp, echo=FALSE---------------------------------------------------
cm_crann$angle.compare

## ----av.dist, echo=FALSE------------------------------------------------------
cm_crann$average.dist

## ----interpolmesh,out.width='75%',fig.align="center",echo=FALSE---------------
 knitr::include_graphics("interpolmesh1.png")

