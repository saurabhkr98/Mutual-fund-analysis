rel<-c(4.83,1.67,0.43,4.35,-0.99,0.51,6.29,1.91,4.34,-1.76,-4.32,-4.73) #rel=mir  #sund+kotak 
sund<-c(3.68,-0.8,0.71,2.59,-0.35,0.42,6.32,2.91,4.05,-3.01,-1.95,-2.33)#bnp=icici
bnp<-c(5.76,2.86,0.89,8.28,-1.97,-2.37,4.12,-0.14,1.04,3.39,-7.13,-2.45)
mr<-19.73
ms<-17.37
mb<-17.42
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


plot(st,er,xaxt='n',yaxt='n')
axis(side=1, at=seq(8,18,0.1))
axis(sid=2, at=seq(17.5,19.5,0.1))
box()

for (i in 1:3)
{
  for (j in 1:3)
  {
    cat (as.numeric(A[i,j]), " ")
  }
  cat("\n")
}
