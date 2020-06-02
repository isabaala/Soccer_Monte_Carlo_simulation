# Soccer championship analysis using MC simulation

# Assumptions :Equality among the team: eveyone has the same probability to win

# Independence between results and games
#-----------------------Create a matrix B(M,N)-------------------------

# Function that asks the number of teams in championship
n=22
i=j=m=n
# random variables between 0 and 1 
B=runif(n*n,0,1)
B<-matrix(B,ncol=n, nrow=n)
# fill the matrix b(i,j), with i>j
b<-matrix(NA,ncol=n,nrow=n)
for (i in 2:n){
  for(j in 1:n){
    if(i>j) {b[i,j]<-B[i,j]}
  }
}

#---------------------- PROBABILITY of draw------------------------------
# the proba of draw function
probas<-function(x,y,z){
 library("triangle")
  v=c(x,y,z)
  v=sort(v)
 
rtriangle(1,a=v[1],b=v[3],c=v[2])

}

# ---------------------filling the matrix A(M,N)----------------------
# probability of a draw based in historical data
p=probas(0.20, 0.24, 0.30)
a<-matrix(0,ncol=n,nrow=n)
# Fill the part inf of the matrix
for (i in 1:n){
    for(j in 1:m){ 
      if(i!=j && i>j){
      if(b[i,j] <= p) {
        a[i,j]=1
      }else if(b[i,j] <=p+(1-p)/2){
        a[i,j]=3
      }else{
          a[i,j]=0}
         
      }
    }
    
  }
 # fill the part sup in the matrix
for (i in 1:n){
  for(j in 1:m){
    if(i>j)
    {
  if(a[i,j]==1){
    a[j,i]=1
  }else if(a[i,j]==3){
    a[j,i]=0
  }else{
    a[j,i]=3
  }
    }}}
# total numbers of points
somme=numeric(n)
for(i in 1:n)
{
  somme[i]=sum(a[,i])
}
# results for each team

results=data.frame(somme)
# order to have the 
results[rev(order(results$somme)),]

