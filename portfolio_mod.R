rel<-c(-0.49,-0.83,1.36,5.83,-1.41,3.71,6.54,2.19,5.7,4.91,0.46,-4.23) #rel=adityabirla io  
                                                                         #sund hdfcbalanced
sund<-c(2.67,2.13,0.7,4.43,-1.1,-1.03,4.77,0.69,2.5,1.25,-2.85,-2.01)#bnp=hdfc equity sav
bnp<-c(1.49,0.73,0.65,2.06,-0.25,0.26,2.26,0.54,1.32,1,-1.58,-0.8)
mr<-14.14
ms<-12.3
mb<-11.14
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
for (w1 in seq(from=0, to=1,by=0.05))
{
  x<-1-w1
  for(w2 in seq(from=0, to = x, by=0.05))
  {
    w3<-x-w2
    #cat(w1, " ", w2, " ",w3, "\n")
    w<-c(w1,w2,w3)
    wt<-t(t(w))
    res<-w %*% A
    res1<-res%*%wt
    st[k]=res1[1,]
    er[k]=w1*mr+w2*ms+w3*mb;
    cat ("\n", "ER= ", er[k],  "SD= ", st[k], "w1 = ", w1, "w2= ", w2, "w3= ", w3, "\n")
    k=k+1;
  }
}
plot(st,er,'b',xaxt='n',yaxt='n')
axis(side=1, at=seq(2,12,0.1))
axis(sid=2, at=seq(11.5,14.5,0.1))
box()
for (i in 1:3)
{
  for (j in 1:3)
  {
    cat (as.numeric(A[i,j]), " ")
  }
    cat("\n")
  }