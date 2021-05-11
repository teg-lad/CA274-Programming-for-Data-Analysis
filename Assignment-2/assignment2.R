#Assignement 2 - Adam Tegart. Using all we have done with 1's and 7's
#to create a model for 2's and 4's



d0=scan("digits/Zl0d.dat",nlines=1000,n=256000)
d1=scan("digits/Zl1d.dat",nlines=1000,n=256000)
d2=scan("digits/Zl2d.dat",nlines=1000,n=256000)
d3=scan("digits/Zl3d.dat",nlines=1000,n=256000)
d4=scan("digits/Zl4d.dat",nlines=1000,n=256000)
d5=scan("digits/Zl5d.dat",nlines=1000,n=256000)
d6=scan("digits/Zl6d.dat",nlines=1000,n=256000)
d7=scan("digits/Zl7d.dat",nlines=1000,n=256000)
d8=scan("digits/Zl8d.dat",nlines=1000,n=256000)
d9=scan("digits/Zl9d.dat",nlines=1000,n=256000)

d=c(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9)
d1=matrix(d1,256,1000)
d2=matrix(d2,256,1000)
d4=matrix(d4,256,1000)
d7=matrix(d7,256,1000)
par(mfrow=c(2,2))
for(i in c(4001:4100))#2001:2100, 
{
  z=matrix(d[,i],16,16)
  image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  readline()
}

#I will find the average 2 and 4
avg_2 <- rep(0, 256)
avg_4 <- rep(0, 256)
for (i in 1:256)
{
  avg_2[i] <- sum(d[i,2001:3000])/1000
  avg_4[i] <- sum(d[i,4001:5000])/1000
}
z=matrix(avg_2,16,16)
z=matrix(avg_4,16,16)
image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
#We can see the average, so lets get a model that roughly fits that

#this function draws a stroke on a 16x16 image
#the arguments are x1,y1 the coordinates of the start point
#theta is its angle
#len is its length and width is its width
stroke = function(x1,y1,theta,len,width)
{
x=c(1:16)
y=c(1:16)

x=x-x1
y=y-y1
u1=x*cos(theta)
u2=y*sin(theta)
u=matrix(u1,16,16)+t(matrix(u2,16,16))


v1=-x*sin(theta)
v2=y*cos(theta)
v=matrix(v1,16,16)+t(matrix(v2,16,16))

m = (v > 0 & v < len)
exp(-u*u/width)*m
}

###############################################################################
params2 = matrix(0,17,10000)
b2=matrix(0,256,10000)
for (i in 1:10000) # Model for 2's
{
repeat{
#this is the upper stroke
x1=runif(1,0.5,6.5)
y1=runif(1,10,15)
if (y1 <= 14)
{
theta1=runif(1,-pi*0.45, -pi*0.23)
len1=runif(1,5,9.5)
}
else
{
len1=runif(1,3.5,8)
theta1=runif(1,-pi*0.5, -pi*0.30)
}
width=runif(1,0.75,3)

#these lines calculate the end point of the cross-stroke
xe2=x1-len1*sin(theta1)
ye2=y1+len1*cos(theta1)

#this is the connecting stroke
x2=runif(1,xe2, xe2 + 0.5)
y2=runif(1,ye2 - 0.5, ye2 + 0.5)
theta2 = runif(1,-pi*0.85,-pi*0.6)
len2=runif(1,5.5,8.5)

xe3=x2-len2*sin(theta2)
ye3=y2+len2*cos(theta2)

#this is the diagonal stroke
x3=runif(1,xe3 - 1, xe3 + 1)
y3=runif(1,ye3 - 1, ye3 + 1)
#x2=runif(1,0.5,5)
#y2=runif(1,0.5,4.5)
theta3 = runif(1,pi*0.6, pi*0.85)
len3=runif(1,13,17)

xe4=x3-len3*sin(theta3)
ye4=y3+len3*cos(theta3)


#this is the bottom stroke
x4=runif(1,xe4 - 3, xe4 + 1)
y4= runif(1,ye4, ye4 + 3)
theta4 = runif(1,-pi*0.65 ,-pi*0.5)
len4=runif(1,8,15)

#these lines calculate the y coordinate of the point on stroke 1
#immediately above the end point of the cross-stroke
#xd=x2-x1
#yb2=y2+xd/tan(theta2)

#this line calculates the vertical separation between the end point
#of the cross-stroke and stoke 1 
#sep = y1-yb2
#print(sep)

if(ye4 >= 0.5)break
}

params2[,i] = c(x1,y1,theta1,len1,x2,y2,theta2,len2,x3,y3,theta3,len3,x4,y4,theta4,len4,width)

s1=stroke(x1,y1,theta1,len1,width)
s2=stroke(x2,y2,theta2,len2,width)
s3=stroke(x3,y3,theta3,len3,width)
s4=stroke(x4,y4,theta4,len4,width)

im=s1+s2+s3+s4
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
#readline()

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b2[,i]=im
}

###################################################################################
params4 = matrix(0,13,10000)
b4=matrix(0,256,10000)
for (i in 1:10000) #Model for 4's
{
repeat{
#this is the right stroke
x1=runif(1,2.5,12.5)
y1=runif(1,0.25,3)
if (x1 <= 8)
{
theta1=runif(1,-pi*0.2,-pi*0.07)
}
else
{
theta1=runif(1,-pi*0.15,-pi*0.02)
}
len1=runif(1,11,15)
width=runif(1,0.75,4)

#these lines calculate the mid point of the first stroke
xe2=x1-len1*sin(theta1)/2
ye2=y1+len1*cos(theta1)/2

#this is the connecting stroke
x2=runif(1,xe2, xe2 + 5)
y2=runif(1,ye2 - 0.5, ye2 + 0.5)
theta2 = runif(1,pi*0.46, pi*0.59)
len2=runif(1,5 + (x2 - x1),10 + (x2 - x1))

xe3=x2-len2*sin(theta2)
ye3=y2+len2*cos(theta2)

#this is the left stroke
x3=runif(1,xe3 - 0.5, xe3 + 0.5)
y3=runif(1,ye3 - 0.5, ye3 + 0.5)
theta3 = runif(1,theta1 - pi*0.04 ,theta1 + pi*0.01)
len3=runif(1,11,19)

#these lines calculate the y coordinate of the point on stroke 1
#immediately above the end point of the cross-stroke
#xd=x2-x1
#yb2=y2+xd/tan(theta2)

#this line calculates the vertical separation between the end point
#of the cross-stroke and stoke 1 
#sep = y1-yb2
#print(sep)

if(xe3 >= 0.5)break
}

params4[,i] = c(x1,y1,theta1,len1,x2,y2,theta2,len2,x3,y3,theta3,len3,width)

s1=stroke(x1,y1,theta1,len1,width)
s2=stroke(x2,y2,theta2,len2,width)
s3=stroke(x3,y3,theta3,len3,width)

im=s1+s2+s3
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
#readline()


im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b4[,i]=im
}
#############################################################################

findnns = function(d,x)
{
  d=d-x
  d=d^2
  dists=apply(d,2,sum)
  dorder=order(dists)
  dorder[1:40]
}

d2=scan("digits/Zl2d.dat",nlines=1000,n=256000)
d4=scan("digits/Zl4d.dat",nlines=1000,n=256000)
d2=matrix(d2,256,1000)
d4=matrix(d4,256,1000)

#this is the same code as we used last week to find the 40 nearest neighbours
#however, this time we are searching matrix b rather than d
nbrs2 = matrix(0,40,1000)
for(i in c(1:1000))nbrs2[,i]=findnns(new_b2,d2[,i])

nbrs4 = matrix(0,40,1000)
for(i in c(1:1000))nbrs4[,i]=findnns(new_b4,d4[,i])

b2=t(b2)
c=var(b)
dim(c)
e=eigen(c)
plot(e$values)
pb= b2 %*% e$vectors[,1:40]
d2=t(d2)
pd=d2 %*% e$vectors[,1:40]


pb=t(pb)
pd=t(pd)

nbrsbox2 = matrix(0,40,1000)
proc.time()
for(i in c(1:1000))
{
#box=which(abs(pb[1,]-pd[1,i]) < 600)
box=which(abs(pb[1,]-pd[1,i]) < 500 & abs(pb[2,]-pd[2,i]) < 500)
nnb=findnns(pb[1:40,box],pd[1:40,i])
nbrsbox2[,i]=box[nnb]
}
proc.time()

b2=t(b2)

nearest = new_b2[,nbrs2[1,]]
dim(nearest)
dists = nearest -d2
dists = apply(dists^2,2,sum)

nearest = new_b4[,nbrs4[1,]]
dim(nearest)
dists = nearest -d4
dists = apply(dists^2,2,sum)


dev.new()
hist(dists,main="Dists for PCA 4's model")

#this code selects extreme bad matches
group = which(dists > dists_real4)
par(mfrow=c(1,3))
for(i in group)
{
z = matrix(d4[,i],16,16) # the real digit
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(b4[,nbrs4[1,i]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(b2[,nbrs_real4[1,i]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
print(i)
print(dists[i])
print(dists_real4[i])
#model2 = sum((d2[,i] - b2[,nbrs2[1,i]])^2)
#model4 = sum((d2[,i] - b4[,nbrs_real2[1,i]])^2)
#if (model2 > model4) print (i)
readline()
}

b=cbind(b2,b4)
d=cbind(d2,d4)

#for each of the real twos and real fours we find the nearest neigbours
#among the 10000 random ones and 10000 random sevens
nbrs24 = matrix(0,40,2000)
for(i in c(1:2000))nbrs24[,i]=findnns(b,d[,i])

#this is an edited version of the classify function from previous weeks
#the labels vector contains only twos and fours
classify = function(nns,k)
{
labels = c(rep("2",10000),rep("4",10000))
digits= labels[nns[1:k]] #we include the first nearest neighbour 
t=table(digits)
m=which.max(t)
names(m)
}

#we calculate the accuraccy for different values of k from 1 to 40
acc=0
labels = c(rep("2",1000),rep("4",1000))
for(k in c(1:40))
{
results = apply(nbrs24,2,function(x) classify(x,1))
t=table(results,labels)
acc[k]=sum(diag(t))
}
plot(c(1:40),acc)
lines(c(1:40),acc)

########################################################
#Section 4 - Looking at parameter values.

nearest = b2[,nbrs2[1,]]
dim(nearest)
dists = nearest -d2
dists = apply(dists^2,2,sum)
goodfit2=nbrs2[1,dists < 2000000]

nearest = b4[,nbrs4[1,]]
dim(nearest)
dists = nearest -d4
dists = apply(dists^2,2,sum)
goodfit4=nbrs4[1,dists < 2000000]

#params2[,i] = c("x1","y1","theta1","len1","x2","y2","theta2","len2","x3","y3","theta3","len3","x4","y4","theta4","len4","width")
#params4[,i] = c(x1,y1,theta1,len1,x2,y2,theta2,len2,x3,y3,theta3,len3,width)

for (i in 1:12){
  for(j in (1+i):13){
  plot(params4[i,goodfit2],params4[j,goodfit2], xlab=rownames(params4)[i], ylab=rownames(params4)[j])
  readline()
  }
}
###########Paramter PCA 2's
gp = params2[,goodfit2]

gp=t(gp)
c=var(gp)
e=eigen(c)
p=gp %*% e$vectors
plot(e$values)
mp=apply(p,2,mean) #computes the mean for each eigenvectors

#this code generated 10000 random values for each eigenvector
#the random values come from a Normal distribtuion
ranv = matrix(rnorm(170000),17,10000)
ranv = ranv * sqrt(e$values) #sets the standard deviation 
ranv = ranv+mp # sets the mean for each eigenvectros

#this line reconstructs the parameter vectors from the #eignvectors
#ranv3 contains the 8 parameters for the model
ranv3=t(ranv)%*% t(e$vectors)

#new_b2 = matrix(0,256,10000)
for(i in c(1:10000))
{
x1=ranv3[i,1]
y1=ranv3[i,2]
theta1=ranv3[i,3]
len1=ranv3[i,4]

x2=ranv3[i,5]
y2=ranv3[i,6]
theta2=ranv3[i,7]
len2=ranv3[i,8]

x3=ranv3[i,9]
y3=ranv3[i,10]
theta3=ranv3[i,11]
len3=ranv3[i,12]

x4=ranv3[i,13]
y4=ranv3[i,14]
theta4=ranv3[i,15]
len4=ranv3[i,16]

width=ranv3[i,17]

s1=stroke(x1,y1,theta1,len1,width)
s2=stroke(x2,y2,theta2,len2,width)
s3=stroke(x3,y3,theta3,len3,width)
s4=stroke(x4,y4,theta4,len4,width)

im=s1+s2+s3+s4
im[im > 1]=1

image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
#new_b2[,i]=im


readline()
}

#######Parameter PCA for model 4's
gp = params4[,goodfit4]

gp=t(gp)
c=var(gp)
e=eigen(c)
p=gp %*% e$vectors
plot(e$values)
mp=apply(p,2,mean) #computes the mean for each eigenvectors

#this code generated 10000 random values for each eigenvector
#the random values come from a Normal distribtuion
ranv = matrix(rnorm(130000),13,10000)
ranv = ranv * sqrt(e$values) #sets the standard deviation 
ranv = ranv+mp # sets the mean for each eigenvectros

#this line reconstructs the parameter vectors from the #eignvectors
#ranv3 contains the 8 parameters for the model
ranv3=t(ranv)%*% t(e$vectors)

#new_b4 = matrix(0,256,10000)
for(i in c(1:10000))
{
x1=ranv3[i,1]
y1=ranv3[i,2]
theta1=ranv3[i,3]
len1=ranv3[i,4]

x2=ranv3[i,5]
y2=ranv3[i,6]
theta2=ranv3[i,7]
len2=ranv3[i,8]

x3=ranv3[i,9]
y3=ranv3[i,10]
theta3=ranv3[i,11]
len3=ranv3[i,12]

width=ranv3[i,13]

s1=stroke(x1,y1,theta1,len1,width)
s2=stroke(x2,y2,theta2,len2,width)
s3=stroke(x3,y3,theta3,len3,width)

im=s1+s2+s3
im[im > 1]=1

image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
#new_b4[,i]=im


readline()
}

############# Now classify them


b=cbind(new_b2,new_b4)
d=cbind(d2,d4)

nbrs24 = matrix(0,40,2000)
for(i in c(1:2000))nbrs24[,i]=findnns(b,d[,i])

#we calculate the accuraccy for different values of k from 1 to 40
acc=0
labels = c(rep("2",1000),rep("4",1000))
for(k in c(1:40))
{
results = apply(nbrs24,2,function(x) classify(x,3))
t=table(results,labels)
acc[k]=sum(diag(t))
}
plot(c(1:40),acc)
lines(c(1:40),acc)

#######
b=cbind(new_b2,new_b4)
d=cbind(d2,d4)
b=t(b)
c=var(b,na.rm=T)
dim(c)
e=eigen(c)
plot(e$values)
pb=b %*% e$vectors
d=t(d)
pd=d %*% e$vectors


pb=t(pb)
pd=t(pd)
indexes = 2
for(i in 1:50) indexes = c(indexes,(i*5))
eacc= rep(0,256)
etime=rep(0,256)

for(n in 32:(length(indexes)-10))
{
  print("starting")
  start=Sys.time()
  for(i in c(1:2000))nbrs24[,i]=findnns(pb[1:indexes[n],],pd[1:indexes[n],i])
  results = apply(nbrs24,2,function(x) classify(x,3))
  t=table(results,labels)
  eacc[indexes[n]]= sum(diag(t))
  full_t=Sys.time()
  etime[indexes[n]] = full_t-start
  print(indexes[n])
}

plot(indexes,eacc[indexes],main="Accuracy for different no. of eigenvectors")
plot(indexes,etime[indexes],main="Time for different numbers of eigenvectors")


########Section 7
params1=matrix(0,8,10000)
b1=matrix(0,256,10000) #in the lecture I used values of 10,000 and 100,000
for(i in c(1:10000))
{
#this code generates random values for each argument of stroke
x1=runif(1,12,16)
y1=runif(1,12,16)
theta1 = runif(1,pi/2,pi*0.9)# new upper limit pi*0.9, old pi*0.75
len=20
width1=runif(1,0.5,3) #in the first lecture the upper limit was 2

x2=runif(1,12,16)
y2=runif(1,12,16)
theta2 = runif(1,theta1,pi*1.1)#new lower limit theta1, old pi*0.75
len=20
width2=runif(1,0.5,3)#new upper limit 3, old 2

params1[,i]=c(x1,y1,theta1,width1,x2,y2,theta2,width2)

s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)

im=s1+s2
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b1[,i]=im


#readline()
}

params7=matrix(0,13,10000)
b7=matrix(0,256,10000)
for(i in c(1:10000))
{
repeat{
#this code generates random values for each argument of stroke
#this is the upper stroke
x1=runif(1,12,16)
y1=runif(1,12,16)
theta1 = runif(1,pi/2,pi*0.65)
len=20
width1=runif(1,0.5,3)

#this is the lower stroke
x2=runif(1,12,16)
y2=runif(1,12,16)
theta2 = runif(1,pi*0.75,pi)
len=20
width2=runif(1,0.5,3)

#this is the cross-stroke
x3=runif(1,13,16)
y3=runif(1,7,9)
theta3 = runif(1,pi*0.45,pi*0.55)
len3=runif(1,8,16)
width3=runif(1,0.5,3)

#these lines calculate the end point of the cross-stroke
xe3=x3-len3*sin(theta3)
ye3=y3+len3*cos(theta3)

#these lines calculate the y coordinate of the point on stroke 1
#immediately above the end point of the cross-stroke
xd=x1-xe3
ye1=y1+xd/tan(theta1)

#this line calculates the vertical separation between the end point
#of the cross-stroke and stoke 1 
sep = ye1-ye3
#print(sep)

if(sep > 3.0)break
}

params7[,i] = c(x1,y1,theta1,width1,x2,y2,theta2,width2,x3,y3,theta3,len3,width3)
s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)
s3=stroke(x3,y3,theta3,len3,width3)

im=s1+s2+s3
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b7[,i]=im

#readline()
}

b=cbind(b1,new_b2,new_b4,b7)
d=cbind(d1,d2,d4,d7)

nbrs1247 = matrix(0,40,4000)
for(i in c(1:4000))nbrs1247[,i]=findnns(b,d[,i])

classify = function(nns,k)
{
labels = c(rep("1",10000),rep("2",10000),rep("4",10000),rep("7",10000))
digits= labels[nns[1:k]] #we include the first nearest neighbour 
t=table(digits)
m=which.max(t)
names(m)
}

#we calculate the accuraccy for different values of k from 1 to 40
acc=0
labels = c(rep("1",1000),rep("2",1000),rep("4",1000),rep("7",1000))
for(k in c(1:40))
{
results = apply(nbrs1247,2,function(x) classify(x,17))
t=table(results,labels)
acc[k]=sum(diag(t))
}
plot(c(1:40),acc)

nbrs1 = matrix(0,40,1000)
for(i in c(1:1000))nbrs1[,i]=findnns(b1,d1[,i])

nbrs7 = matrix(0,40,1000)
for(i in c(1:1000))nbrs7[,i]=findnns(b7,d7[,i])

nearest = b1[,nbrs1[1,]]
dim(nearest)
dists = nearest -d1
dists = apply(dists^2,2,sum)
hist(dists, main = "Dists for 1's")

nearest = b7[,nbrs7[1,]]
dim(nearest)
dists = nearest -d7
dists = apply(dists^2,2,sum)
hist(dists, main = "Dists for 7's")

#this code selects extreme bad matches
group = which(results==4 & labels == 7)
par(mfrow=c(1,1))
for(i in group)
{
z = matrix(d[,i],16,16) # the real digit
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(b[,nbrs1247[1,i]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(b7[,nbrs7[1,i%%1000]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
#model2 = sum((d2[,i] - b2[,nbrs2[1,i]])^2)
#model4 = sum((d2[,i] - b4[,nbrs_real2[1,i]])^2)
#if (model2 > model4) print (i)
readline()
}

goodfit1=nbrs1[1,dists < 1250000]
goodfit7=nbrs7[1,dists < 2000000]
gp=params1[,goodfit1]

gp=t(gp)
c=var(gp)
e=eigen(c)
p=gp %*% e$vectors
plot(e$values)
mp=apply(p,2,mean) #computes the mean for each eigenvectors

#this code generated 10000 random values for each eigenvector
#the random values come from a Normal distribtuion
ranv = matrix(rnorm(80000),8,10000)
ranv = ranv * sqrt(e$values) #sets the standard deviation 
ranv = ranv+mp # sets the mean for each eigenvectros

#this line reconstructs the parameter vectors from the #eignvectors
#ranv3 contains the 8 parameters for the model
ranv3=t(ranv)%*% t(e$vectors)


plot(ranv3[,1],ranv3[,2])
plot(ranv3[,3],ranv3[,7])

new_b1=matrix(0,256,10000)
for(i in c(1:10000))
{
x1=ranv3[i,1]
y1=ranv3[i,2]
theta1=ranv3[i,3]
width1=ranv3[i,4]

x2=ranv3[i,5]
y2=ranv3[i,6]
theta2=ranv3[i,7]
width2=ranv3[i,8]

s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)

im=s1+s2
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
new_b1[,i]=im


#readline()
}

gp=params7[,goodfit7]

gp=t(gp)
c=var(gp)
e=eigen(c)
p=gp %*% e$vectors
plot(e$values)
mp=apply(p,2,mean) #computes the mean for each eigenvectors

#this code generated 10000 random values for each eigenvector
#the random values come from a Normal distribtuion
ranv = matrix(rnorm(130000),13,10000)
ranv = ranv * sqrt(e$values) #sets the standard deviation 
ranv = ranv+mp # sets the mean for each eigenvectros

#this line reconstructs the parameter vectors from the #eignvectors
#ranv3 contains the 8 parameters for the model
ranv3=t(ranv)%*% t(e$vectors)


plot(ranv3[,1],ranv3[,2])
plot(ranv3[,3],ranv3[,7])

new_b7=matrix(0,256,10000)
for(i in c(1:10000))
{
x1=ranv3[i,1]
y1=ranv3[i,2]
theta1=ranv3[i,3]
width1=ranv3[i,4]

x2=ranv3[i,5]
y2=ranv3[i,6]
theta2=ranv3[i,7]
width2=ranv3[i,8]

x3=ranv3[i,9]
y3=ranv3[i,10]
theta3=ranv3[i,11]
len3=ranv3[i,12]
width3=ranv3[i,13]


s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)
s3=stroke(x3,y3,theta3,len3,width3)

im=s1+s2+s3
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
new_b7[,i]=im


#readline()
}

new_nbrs1 = matrix(0,40,1000)
for(i in c(1:1000))new_nbrs1[,i]=findnns(new_b1,d1[,i])

new_nbrs7 = matrix(0,40,1000)
for(i in c(1:1000))new_nbrs7[,i]=findnns(new_b7,d7[,i])

nearest = new_b1[,new_nbrs1[1,]]
dim(nearest)
dists = nearest -d1
dists = apply(dists^2,2,sum)
hist(dists, main = "Dists for new 1's")

nearest = new_b7[,new_nbrs7[1,]]
dim(nearest)
dists = nearest -d7
dists = apply(dists^2,2,sum)
hist(dists, main = "Dists for new 7's")

b=cbind(new_b1,new_b2,new_b4,new_b7)
d=cbind(d1,d2,d4,d7)

new_nbrs1247 = matrix(0,40,4000)
for(i in c(1:4000))new_nbrs1247[,i]=findnns(b,d[,i])

classify = function(nns,k)
{
labels = c(rep("1",10000),rep("2",10000),rep("4",10000),rep("7",10000))
digits= labels[nns[1:k]] #we include the first nearest neighbour 
t=table(digits)
m=which.max(t)
names(m)
}

#we calculate the accuraccy for different values of k from 1 to 40
acc=0
labels = c(rep("1",1000),rep("2",1000),rep("4",1000),rep("7",1000))
for(k in c(1:40))
{
results = apply(new_nbrs1247,2,function(x) classify(x,5))
t=table(results,labels)
acc[k]=sum(diag(t))
}
plot(c(1:40),acc)

#this code selects extreme bad matches
group = which(results==2 & labels == 7)
par(mfrow=c(1,3))
for(i in group)
{
z = matrix(d[,i],16,16) # the real digit
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(b[,new_nbrs1247[1,i]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
z = matrix(new_b7[,nbrs7[1,i%%1000]],16,16) #the nearest neighbour in matrix b
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
#model2 = sum((d2[,i] - b2[,nbrs2[1,i]])^2)
#model4 = sum((d2[,i] - b4[,nbrs_real2[1,i]])^2)
#if (model2 > model4) print (i)
readline()
}

