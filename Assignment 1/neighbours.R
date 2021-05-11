#this file contains all the code for running the examples in
#Lecture Slides on kNN and the Video from March 24

setwd("D://DS/DS2/CA274 - Programming for Data Analytics/Assignment 1/")
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
d=matrix(d,256,10000)

nbrs256=read.csv(file="nbrs256.csv", row.names=1)
eigen=read.csv(file="eigen.csv", row.names=1)
eigen=eigen[[1]]

#this function finds the 40 nearest neighbours of x
#x is a single image
#d is a dataset containing many images
findnns = function(d,x)
{
  d=d-x
  d=d^2
  dists=apply(d,2,sum)
  dorder=order(dists)
  dorder[1:40]
}

#this function finds the most frequent class among the k nearest neighbours
#nns is a vector containing the indices of the 40 nearest neighbours
#k is an integer from 2 to 40
classify = function(nns,k)
{
  labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
  digits= labels[nns[2:k]]
  t=table(digits)
  m=which.max(t)
  names(m)
}


#these two lines calculate the 40 nearest neighbours for each of the 10000 digits
nbrs256 = matrix(0,40,10000)
system.time(for(i in c(1:10000))nbrs256[,i]=findnns(d,d[,i]))
write.csv(nbrs256, file = "nbrs256.csv")

#The following code is for getting the nearest neighbours for k=2
results = apply(nbrs256,2,function(x) classify(x,2))
t=table(results,labels)

group = which(labels == "8" & results == "3")
par(mfrow=c(1,3))
for(i in group)
{
  curr = c(i, nbrs256[2,i], nbrs256[4,i])
  for(i in curr)
  {
    z=matrix(d[,i],16,16)
    image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  }
  readline()
}

#this loop runs the kNN algorihm for different values of k from 2 to 40
#for each value of k it classifies each of the 10000 digits
#the output is stored in the vector 'results'
#the vector 'labels' contains the true classifications for each of the 10000 digits
#'t' is a confusion matrix 
#the elements on the diagonal of t are the digits which have been correctly classified
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
acc=0
time=0

for(k in c(2:40))
{
  start=Sys.time()
  results = apply(nbrs256,2,function(x) classify(x,k))
  end=Sys.time()
  t=table(results,labels)
  acc[k]=sum(diag(t))
  time[k] = end-start
}
plot(c(2:40),acc[2:40])
plot(c(2:40),time[2:40])

#this line selects the 8s which are misclassified as 3s
group = which(labels == "8" & results == "3")
group = 8270
#this code displays the true digit next to its nearest neighbour
par(mfrow=c(1,2))
for(i in group)
{
  z=matrix(d[,i],16,16)
  image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  z=matrix(d[,nbrs256[4,i]],16,16)
  image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  print(i)
  print(nbrs256[4, i])
  readline()
}
#Section 2
theta = 0.05
rot=matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
rev=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2)

#this is the code to calculate the eigenvectors
d=t(d)
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors
to_plot = c(c(3001:4000),c(8001:9000))

proj=matrix(0,2,2000)
colours=c(rep("red",1000),rep("lightblue",1000))
plot_char=c(rep(19,2000))
for(i in group)
{
  plot_char[i-7000] = 8
  colours[i-7000] = "black"
}
plot_char
p=t(p)
for(i in c(1:3000))
{
  
  pos = locator(1)
  
  if(pos$x < 0)
  {
    if(pos$y < 0) p[c(1,3),] = rot %*% p[c(1,3),] else  p[c(1,2),] = rot %*% p[c(1,2),]
  }
  else
  {
    if(pos$y < 0) p[c(1,3),] = rev %*% p[c(1,3),] else p[c(1,2),] = rev %*% p[c(1,2),]
  }
  
  
  proj[1,] = p[1,to_plot]*4000/(9000-p[3,to_plot])
  proj[2,] = p[2,to_plot]*4000/(9000-p[3,to_plot])
  
  zorder = order(p[3,to_plot])
  plot(proj[1,zorder],proj[2,zorder],xlim=c(-1000,1000),ylim=c(-1000,1000),col=colours[zorder],pch=plot_char[zorder])
}

#Section 3
#these two lines calculate the 40 nearest neighbours using the first 20 eigenvectors
p=t(p)
nbrsp20 = matrix(0,40,10000)
proc.time()
for(i in c(1:10000))nbrsp20[,i]=findnns(p,p[,i]) #removed p[1:20,] and p[1:20,i]]
proc.time()

#to calculate the accuracy we use the same code as above but we replace 'nbrs256' with 'nbrsp20'
pacc=0
for(k in c(2:40))
{
  results = apply(nbrsp20,2,function(x) classify(x,k))
  t=table(results,labels)
  pacc[k]=sum(diag(t))
}
plot(c(2:40),pacc[2:40])


#this code calculates the nearest neighbours and the accuracy
#for different numbers of eigenvectors from 2 to 20
#this code takes a very long time to run!

pacc=0
pacc=eigen
neigh=0
full=0
neighbours = matrix(0,40,10000)

for(n in 34:34)
{
  print("starting")
  start=Sys.time()
  for(i in c(1:10000))neighbours[,i]=findnns(p[1:n,],p[1:n,i])
  neigh_t=Sys.time()
  results = apply(neighbours,2,function(x) classify(x,5))
  t=table(results,labels)
  check= sum(diag(t))
  full_t=Sys.time()
  ##neigh= c(neigh,neigh_t-start)
  check_time = c(full,full_t-start)
  print(n)
}
index=c(1:150)
for(i in 1:((256-150)/2))
{
index[i + 150] = (i * 2) + 150
}
plot(index,pacc)
eigen_acc <- pacc
write.csv(eigen_acc, file="eigen.csv")

test <- c(2:10)
rest=0
for(i in c(1:((256-10)/5)))
{
  rest[i] <- (i * 5) + 10
}
test = c(test,rest,256)
write.csv(full, "eigen_times.csv") # contains 2:10 then in steps of 5 to 255, then 256 and 34
plot(test,full,xlab="No. of eigenvectors", ylab="Time in minutes")

eigen[which.max(eigen)]

#Section 4
#this code calcuates the 40 nearest neighbours by searching within a box
nbrsbox = matrix(0,40,10000)
box_dim=c(200,300,400,500,600,700,800,900)
box_times=matrix(0,8,8)
boxacc=matri=matrix(0,8,8)

for(j in 1:(length(box_dim)-1))
{
  for(k in 8:8) 
  {
    start=Sys.time()
    for(i in c(1:10000))
    {
      #box=which(abs(p[1,]-p[1,i]) < 600)
      box=which(abs(p[1,]-p[1,i]) < box_dim[j] & abs(p[2,]-p[2,i]) < box_dim[k])
      nnb=findnns(p[1:20,box],p[1:20,i])
      nbrsbox[,i]=box[nnb]
    }
  print("neighbours found")
  results = apply(nbrsbox,2,function(x) classify(x,5))
  end=Sys.time()
  t=table(results,labels)
  boxacc[j,k]=sum(diag(t))
  box_times[j,k] = end-start
  print(end-start)
  print(box_dim[j])
  print(box_dim[k])
  print("next")
  }
}

boxtmp=box_times
for (k in 1:8)
{
  for (j in 1:8)
  {
    if (boxtmp[j,k] > 10)
      boxtmp[j,k] = boxtmp[j,k]/60
  }
} 
plot(boxtmp[1,1],boxacc[1,1],main="Accuracy at different dimensions",xlab="Time taken", ylab="Accuracy", col="red",xlim=c(0.25,1.5), ylim=c(9630,9740))

for (k in 1:8)
{
  for (j in 1:8)
  {
    points(boxtmp[k,j],boxacc[k,j],col=colours[j])
    print("added")
    print(k)
    print(j)
  }
}
group=identify(boxtmp,boxacc,n=6)
colours=c("red","blue","green","blueviolet","chocolate","cyan","magenta","brown")
which.max(boxacc)
#we can check how similar the neighbours are to the neighbours calculated with the full dataset
sum(nbrsp20[1:5,] != nbrsbox[1:5,])

#we could also calculate the accuracy with the same code as for 'nbrs256'
#but we would substitute 'nbrs256' with 'nbrsbox' 

pacc=0
for(k in c(2:40))
{
  results = apply(nbrsbox,2,function(x) classify(x,k))
  t=table(results,labels)
  pacc[k]=sum(diag(t))
}
points(c(2:40),pacc[2:40],col='green')

#Section 5
blurring=c(1,3,5,7,9,11)
blracc=list()
blrtime=list()
box_dim=list()
box_dim[[1]] = c(400,500,600,700,800,900)
box_dim[[2]] = c(400,500,600,700,800,900)
box_dim[[3]] = c(300,400,500,600,700,800)
box_dim[[4]] = c(300,400,500,600,700,800)
box_dim[[5]] = c(300,400,500,600,700,800)
box_dim[[6]] = c(300,400,500,600,700,800)

for(l in 5:length(box_dim))
{
blracc[[l]] = matrix(0,6,6)
rownames(blracc[[l]]) = box_dim[[1]]
colnames(blracc[[l]]) = box_dim[[1]]
blrtime[[l]] = matrix(0,6,6)
rownames(blrtime[[l]]) = box_dim[[1]]
colnames(blrtime[[l]]) = box_dim[[1]]
#this is the code to decide how much the images are blurred
width = 1##blurring[l]
ii = c(0:15)
ii[9:16] = 17 - c(9:16)
ii = ii^2
ex = exp(-ii/width)
gau = ex %*% t(ex)
image(c(1:16),c(1:16),256*gau,col=gray(c(0:256)/256))


d=c(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9)
d=matrix(d,256,10000)
d=t(d)

#this is the code that does the blurring
#the blurred images are called x2
x2=d
x=d
for(i in c(1:10000))
{
  z = matrix(as.numeric(x[i,]),16,16)
  ft=fft(z)
  ft = ft *gau
  z2=fft(ft,inverse=T)
  #image(c(1:16),c(1:16),abs(z2),col=gray(c(0:256)/256))
  x2[i,]=matrix(abs(z2),1,256)
}

#this sets the mean of the blurred data to zero
mx2=apply(x2,2,mean)
x2=sweep(x2,2,mx2)
x2=256*x2/max(x2)

#this calculates the eigenvectors of the blurred data
c=var(x2)
e=eigen(c)
plot(e$values)
pblr=x2 %*% e$vectors[,1:3]

#this calculates the nearest neighbours within a box determined by the blurred data
pblr=t(pblr)
nbrsblrbox = matrix(0,40,10000)
for (j in 1:length(box_dim))
{
  for(k in 1:length(box_dim))
  { 
    start=Sys.time() 
    for(i in c(1:10000))
    {
  
      box=which(abs(pblr[1,]-pblr[1,i]) < box_dim[[l]][j] & abs(pblr[2,]-pblr[2,i]) < box_dim[[l]][k])
  
      ##if(length(box) < 100)
      ##  box=index[abs(pblr[1,]-pblrbox[1,i]) < 500 & abs(pblr[2,]-pblr[2,i]) < 500]
  
      #we use the unblurred eigenvectors to find the neighbours
      nnb=findnns(p[1:20,box],p[1:20,i])
      nbrsblrbox[,i]=box[nnb]
    }
    print("neighbours found")
    results = apply(nbrsblrbox,2,function(x) classify(x,5))
    end=Sys.time()
    t=table(results,labels)
    blracc[[l]][j,k]=sum(diag(t))
    blrtime[[l]][j,k] = end-start
    print(box_dim[[l]][j])
    print(box_dim[[l]][k])
    print("next")
  }
}
}
#Again we can find the accuracy using the same code as for 'nbrs256' 
#except we substiute 'nbrsblrbox'

pacc=0
for(k in c(2:40))
{
  results = apply(nbrsblrbox,2,function(x) classify(x,k))
  t=table(results,labels)
  pacc[k]=sum(diag(t))
}
points(c(2:40),pacc[2:40],col='magenta')

colours = c("red", "blue", "green", "magenta", "chocolate", "black")
plot(tmp[[1]][1,1],blracc[[1]][1,1],main="Blurring at different widths",xlab="Time taken", ylab="Accuracy", col=colours[1],ylim=c(9600,9750),xlim=c(0.3,1.5))
for (i in 1:length(blurring))
{
  for (k in 1:6)
  {
    for (j in 1:6)
    {
      points(tmp[[i]][k,j],blracc[[i]][j,k], col=colours[i])
    }
  } 
}
tmp = blrtime
for (i in 1:length(blurring))
{
  for (k in 1:6)
  {
    for (j in 1:6)
    {
      if (blrtime[[i]][j,k] > 10)
        blrtime[[i]][j,k] = blrtime[[i]][j,k]
    }
  } 
}

plot(tmp[[1]][1,1],blracc[[1]][1,1],main="Accuracy at different dimensions of blur width 1",xlab="Time taken", ylab="Accuracy", col="red",xlim=c(0.6,1.6), ylim=c(9600,9750))

for (k in 1:6)
{
  for (j in 1:6)
  {
    points(tmp[[1]][k,j],blracc[[1]][k,j],col=colours[j])
  }
}
group=identify(tmp[[1]],blracc[[1]],n=6)




