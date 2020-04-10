aov.perm<-function(x,y,B=1000,balanced=TRUE){

N<-table(y)
C=length(N)

K=C*(C-1)/2

P.part<-array(0,dim=c(K,1))   

name<-character()

cont=1
for(i in 1:(C-1)){
for(j in (i+1):C){
name[cont]<-paste(unique(y[unclass(y)==i]),unique(y[unclass(y)==j]),sep="-")
cont=cont+1
}
}



sig<-function(p){

s<-character(length=dim(p)[1])

for(i in 1:dim(p)[1]){
if(p[i]<0.1){
s[i]="."
if(p[i]<0.05){
s[i]="*"
if(p[i]<0.01){
s[i]="**"
if(p[i]<0.001){
s[i]="***"

}
}
}
}
}
return(s)
}## end sig.


T<-array(0,dim=c((B+1),K))
Diff<-T[1,]

cont=1
for(i in 1:(C-1)){
for(j in (i+1):C){
T[1,cont]<-N[i]*N[j]*(mean(x[unclass(y)==i])-mean(x[unclass(y)==j]))^2
Diff[cont]<-mean(x[unclass(y)==i])-mean(x[unclass(y)==j])
cont=cont+1
}
}

for(bb in 2:(B+1)){


if(balanced==TRUE){
n<-length(x)/C
X<-array(0,dim=c((2*n),K))

cont=1
for(i in 1:(C-1)){
for(j in (i+1):C){
X[,cont]=c(x[unclass(y)==i],x[unclass(y)==j])
cont=cont+1
}
}

X.perm=X[sample(seq(1,(2*n))),]                                 

cont=1
for(i in 1:(C-1)){
for(j in (i+1):C){
T[bb,cont]= N[i]*N[j]*(mean(X.perm[c(1:n),cont])-mean(X.perm[-c(1:n),cont]))^2  ##2nd sol
cont=cont+1
}
}
}###End balanced = TRUE




if(balanced==FALSE){

x.perm<-sample(x)

cont=1
for(i in 1:(C-1)){
for(j in (i+1):C){
T[bb,cont]<-N[i]*N[j]*(mean(x.perm[unclass(y)==i])-mean(x.perm[unclass(y)==j]))^2
cont=cont+1
}
}

}##End balanced = FALSE

}#end bb



T1<-apply(T,1,sum)

P.part[,1]<-apply(T,2,function(x){sum(x[-1]>=x[1])/B})
P.part<-data.frame(Diff=Diff,p=P.part,sig.=sig(P.part))
rownames(P.part)<-name

P.glob<-sum(T1[-1]>=T1[1])/B

#P.part<-t2p(T)[1,]
#P.glob<-t2p(T1)[1]

return(list(Global.p.value=P.glob,Partial.p.value=P.part))
cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ")
}