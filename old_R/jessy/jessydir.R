jessydir<-function(){
z=dir(".")
runrec=matrix(c(0,0,0,0,0),1,5)
d=c(0)
nfiles=length(z)
ires=0
d[1]='$\\\\'
d[2]='\\begin{array}{lrrrl}'
d[3]=paste('Model','&','Number','&','Log-','&','Number','&','Date\\\\')
d[4]=paste(' ','&','of','&','likelihood','&','of','&',' \\\\')
d[5]=paste(' ','&','Parameters','&',' ','&','Draws','&',' \\\\')
print(d[1])
for (i1 in 1:nfiles){
a=z[i1]
b=strsplit(a,"")
c=b[[1]]
if (c[1]=="R"){
if (c[2]=="e"){
if (c[3]=="s"){
if (c[4]=="u"){
if (c[5]=="l"){
if (c[6]=="t"){
if (c[7]=="1"){
ires=ires+1
load(a)
runrec[ires,1]=modelnamein
runrec[ires,2]=length(loglik1$estimate)
runrec[ires,3]=round(loglik1$minimum,4)
runrec[ires,4]=modelin$ndraws
runrec[ires,5]=results$datefinish
print(runrec)
d[ires+5]=paste(runrec[ires,1],'&',runrec[ires,2],'&',runrec[ires,3],'&',runrec[ires,4],'&',runrec[ires,5],'\\\\')
}}}}}}}
}
d[ires+6]='\\end{array}'
d[ires+7]='$\\\\'
write(d,'runrec.txt')
runrec
}