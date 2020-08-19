rel<-c(0.49,0.58,0.53,0.55,0.53,0.52,0.53,0.51,0.54,0.54,0.50,0.67)
sund<-c(0.46,0.70,0.73,0.89,0.61,0.29,0.68,0.29,0.15,0.46,0.32,0.81)
bnp<-c(0.50,0.59,0.54,0.56,0.60,0.51,0.57,0.53,0.55,0.56,0.51,0.60)
mr<-7.27
ms<-8.17
mb<-7.14
crs<-cov(rel,sund)
crb<-cov(rel,bnp)
csb<-cov(sund,bnp)
crr<-cov(rel,rel)
cbb<-cov(bnp,bnp)
css<-cov(sund,sund)
st<-c(1:231)
er<-c(1:231)                                                                                               
k<-1
#cat(crs, "    ", crb,"  ", csb,"\n")
A<-matrix(c(crr,crs,crb,crs,css,csb,crb,csb,cbb),nrow=3, ncol=3, byrow=TRUE)
for (w1 in seq(from=0, to=1,by=0.01))
{
  x<-1-w1
  for(w2 in seq(from=0, to = x, by=0.01))
  {
    w3<-x-w2
    #cat(w1, " ", w2, " ",w3, "\n")
    w<-c(w1,w2,w3)
    wt<-t(t(w))
    res<-w %*% A
    res1<-res%*%wt
    st[k]=sqrt(res1[1,])
    er[k]=w1*mr+w2*ms+w3*mb;
    cat ("\n", "ER= ", er[k],  "SD= ", st[k], "w1 = ", w1, "w2= ", w2, "w3= ", w3, "\n")
  k=k+1;
  }
}
plot(st,er,'l')
#(side=1, at=seq(0,0.05,0.0005))
#axis(side=2, at=seq(7,8.2,0.1))
box()
for (i in 1:3)
{
  for (j in 1:3)
  {
    #cat (as.numeric(A[i,j]), " ")
  }
  cat("\n")
}

