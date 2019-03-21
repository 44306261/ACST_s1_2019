#Q1
#n is the total number of coupon payments received from bond
#P=C*exp(-y[1]*t[1])+C*exp(-y[2]*t[2])+...+F*exp(-y[t[n]]*t[n])+P
#y[j]is the interest rate of jth coupon
#t[j]=j/2 is the time until jth couponis paid ( in years)

PVfunction=function(C,F,yield,n){
    #yield=c(interests)
     P=0
     t<-seq(1/2,n/2,0.5)
  for(j in 1:n)
    {
      P=C*exp(-yield[j]*t[j])+P
    }
  P1<-P+F*exp(-yield[n]*t[n])
  return(P1)
}
#PVfunction(6000,100000,c(0.045,0.09,0.14,0.19,0.24,0.3),6)
#answer [1] 66706.02