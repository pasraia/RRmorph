#' @importFrom stats quantile
#' @importFrom Morpho restoreShapes rmUnrefVertex
#' @importFrom Rvcg vcgArea vcgBallPivoting

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
unitV <- function(x) sum(x^2)^0.5
rad2deg <- function(rad)  (rad * 180)/(pi)

areadiff<-function(mesh1,mesh2,out.rem=FALSE,scale01=TRUE,fact=1.5){

  area_shape1<-vcgArea(mesh1,perface=T)$pertriangle
  area_shape2<-vcgArea(mesh2,perface=T)$pertriangle
  diff_areas<-(area_shape1-area_shape2)/area_shape1
  sel<-which(is.na(diff_areas))

  if(length(sel)>0){
    mesh1$it<-mesh1$it[,-sel]
    mesh2$it<-mesh2$it[,-sel]
    mesh1<-rmUnrefVertex(mesh1)
    mesh2<-rmUnrefVertex(mesh2)
    area_shape1<-vcgArea(mesh1,perface=T)$pertriangle
    area_shape2<-vcgArea(mesh2,perface=T)$pertriangle
    diff_areas<-(area_shape1-area_shape2)/area_shape1
  }

  if(out.rem==TRUE){
    x=diff_areas
    qq <- quantile(x, c(1,3)/4, names=FALSE)
    r <- diff(qq) * fact
    tst <- x < qq[1] - r | x > qq[2] + r
    tstp<-qq[2] + r
    tstn<-qq[1] - r
    diff_areas[x>tstp]<-tstp
    diff_areas[x<tstn]<-tstn
  }else diff_areas=diff_areas

  if(scale01==TRUE) diff_areas<-range01(diff_areas)

  return(list("ash1"=area_shape1,"ash2"=area_shape2,"dareas"=diff_areas))
}

symdistr<-function(a) {
  maxa<-which.max(abs(range(a,na.rm = TRUE)))
  if(maxa==1) {
    l<-c(a,a[which(a<(max(a,na.rm = TRUE)*-1))]*-1)
  } else {
    l<-c(a[which(a>(min(a,na.rm = TRUE)*-1))]*-1,a)
  }

  return(list(l=l,maxa=maxa))

}
