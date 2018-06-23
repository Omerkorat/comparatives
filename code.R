# Load data
cwd = NULL # Set this to the current directory
nums.path = file.path(cwd,'data.csv')
nums.data = read.csv(nums.path)



# Subsample data for dev
samp.size = floor(.5*nrow(nums.data))
train.ind = sample(seq.len(nrow(nums.data)),size=samp.size)
nums.data = nums.data[train.ind,]

N = nrow(nums.data)

# Roundness

library(metap)
# This is the hypothesized rounding expression. 
# result of 0 means doesn't satisfy, 1 means satisfies only alternative hyp,
# 2 means satsifies hyp and alternative hyp
islingnat = function(x){
  if (round(x)!=x){
    return (0)
  }
  if (x==0){
    return(0)
  }
  if (x<10){
    if (x==5) {
      return (2)
    } else{
      return(0)
    }
  }
  power = 10**floor(log10(x))
  vestige = x%%power
  if (vestige==0){
    return (1)
  } else {
    if (vestige==power/2){
      return (2)
    } else {
      return (0)
    }
  }
  
}

# These are all the roundness hypotheses
mod10.num = nums.data$num%%10==0
mod5.num = nums.data$num%%5==0
log.num = log10(nums.data$num)
hyp.num = islingnat(nums.data$num)>0
hyp.num.alt = islingnat(nums.data$num)==1

# Collect chisq values for mod10 and mod5:
mean.mod10 = mean(mod10.num)
mean.mod5 = mean(mod5.num)
O.mod10 = mean.mod10*sqrt(mean.mod10*(1-mean.mod10))
O.mod5 = mean.mod5*sqrt(mean.mod5*(1-mean.mod5))

E.mod10 = .1
E.mod5 = .2

chisq.mod10 = (E.mod10-O.mod10)^2/E.mod10
chisq.mod5 = (E.mod5-O.mod5)^2/E.mod5

# Collect chisq values for log, hyp and hyp.alt
ps.log10 = c()
ps.hyp = c()
ps.hyp2 = c()
chisqs.log10 = c()
chisqs.hyp = c()
chisqs.hyp2 = c()

collect.roundness.chisqs = function(){
  for (i in c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)){
    obs = (nums.data$num>=10^i)&(nums.data$num<10^(i+1))
    stdev = sqrt(mean(obs)*(1-mean(obs)))
    O = mean(obs)*stdev
    O.hyp = mean(hyp.num & obs)*stdev
    O.hyp2 = mean(hyp.num.alt & obs)*stdev
    
    wtd.samp.space = ((10^(i+1)-10^i)*O)
    E = 1/wtd.samp.space
    E.hyp = 19/wtd.samp.space
    E.hyp2 = 9/wtd.samp.space
    
    chi2.log10 = (O-E)^2/E
    chi2.hyp = (O.hyp-E.hyp)^2/E.hyp
    chi2.hyp2 = (O.hyp2-E.hyp2)^2/E.hyp2
    
    chisqs.log10 = append(chisqs.log10,chi2.log10)
    chisqs.hyp = append(chisqs.hyp,chi2.hyp)
    chisqs.hyp2 = append(chisqs.hyp2,chi2.hyp2)
    
    p.log10 = pchisq(chi2.log10,df=1) 
    p.hyp =  pchisq(chi2.hyp,df=1) 
    p.hyp2 =  pchisq(chi2.hyp2,df=1)     
    
    ps.log10 = append(ps.log10,p.log10)
    ps.hyp = append(ps.hyp,p.hyp)
    ps.hyp2 = append(ps.hyp2,p.hyp2)
     
  }
  # print mean chisq value for each hyp
  print("mean of chisqs.log10")
  print(mean(chisqs.log10))
  print("mean of chisqs.hyp")
  print(mean(chisqs.hyp))
  print("mean of chisqs.hyp2")
  print(mean(chisqs.hyp2))
  # Combine p values
  return (c(sumlog(ps.log10)$p,sumlog(ps.hyp)$p, sumlog(ps.hyp2)$p))
}

print(chisq.mod10)
print(chisq.mod5)

# Get chisq values for other hyps
ps = collect.roundness.chisqs()

# Get p values
p.log10 = ps[1]
p.hyp =  ps[2]
p.hyp2 =  ps[3]
p.mod10 = pchisq(chisq.mod10,df=1) 
p.mod5 =  pchisq(chisq.mod5,df=1) 


# Print p values
print(p.log10)
print(p.hyp)
print(p.hyp2)
print(p.mod10)
print(p.mod5)


# Comparatives:

# Coefficients:

# Subsample data (optional)
samp.size = floor(.7*nrow(nums.data))
train.ind = sample(seq.len(nrow(nums.data)),size=samp.size)
nums.data = nums.data[train.ind,]


# Create model variables:

# Inputs 
mt.neg = (nums.data$mt & nums.data$neg) + 0 # no more than
mt.pos = (nums.data$mt & !nums.data$neg) + 0 # more than
al = nums.data$al # at least
lt.neg = (nums.data$lt & nums.data$neg) + 0 # no less than
lt.pos = (nums.data$lt & !nums.data$neg) + 0 # noless than
am = nums.data$am # at most
is.p = nums.data$is.p # is percentage
is.frac = nums.data$is.frac # is fraction
oom = floor(log10(nums.data$num)) # order of magnitude
# Targets
num = nums.data$num # number 
log.num = log(nums.data$num) # log number
resp = log10(nums.data$num) - floor(log10(nums.data$num)) # respective size within oom



# Means
mt.pos.num = log(nums.data$num[which(nums.data$mt==1&nums.data$neg==0)])
mt.neg.num = log(nums.data$num[which(nums.data$mt==1&nums.data$neg==1)])
al.num = log(nums.data$num[which(nums.data$al==1)])
am.num = log(nums.data$num[which(nums.data$am==1)])
lt.pos.num = log(nums.data$num[which(nums.data$lt==1&nums.data$neg==0)])
lt.neg.num = log(nums.data$num[which(nums.data$lt==1&nums.data$neg==1)])


sdmean = function(X){
  print(mean(X))
  print(sd(X))
  print(length(X))
}

sdmean(mt.pos.num)
sdmean(mt.neg.num)
sdmean(al.num)
sdmean(am.num)
sdmean(lt.pos.num)
sdmean(lt.neg.num)


# Mean comparisons  (Table 1 in the paper):

# mt vs al:
t.test(mt.pos.num, al.num)

# at most vs less than
t.test(am.num, lt.pos.num)

# Build linear models:

# Predict log numerical value (Table 2 in the paper)
model1 = lm(log.num ~ mt.neg + mt.pos + al + lt.pos + lt.neg + am + is.p + is.frac)
summary(model1)

# Extra model (not in the paper; pretty much equivalent to model2)
model2 = lm(resp ~ mt.neg + mt.pos + al + lt.pos + lt.neg + am + is.p + is.frac)
# test coefficients contribution; this might cause a memory error (failed to allocate vector of size ...). 
# If it does, clear memory by closing other processes
model2=step(model2,direction='both')
summary(model2)



# Plot (FIgures 1-2)

log.num = log(nums.data$num)
mt.pos.num = log(nums.data$num[which(nums.data$mt==1&nums.data$neg==0)])
mt.neg.num = log(nums.data$num[which(nums.data$mt==1&nums.data$neg==1)])
al.num = log(nums.data$num[which(nums.data$al==1)])
am.num = log(nums.data$num[which(nums.data$am==1)])
lt.pos.num = log(nums.data$num[which(nums.data$lt==1&nums.data$neg==0)])
lt.neg.num = log(nums.data$num[which(nums.data$lt==1&nums.data$neg==1)])

col1= rgb(1,0,0,.5)
col2 = rgb(0,0,1,.5)
col3 = rgb(0,1,0,.5)
par(mar=c(4,4,4,4),mfrow=c(1,1))


# Plot at least, more than and less than
hist(mt.pos.num,col=col1,xlim=c(0,20),
     ylim=c(0,20000),xaxt="n",main='',
     ylab='',xlab='',yaxt='n',add=F,
     breaks=30)
hist(al.num,col=col2,add=T,breaks=30)
hist(lt.pos.num,col=col3,add=T,breaks=30)
axis(side = 1, at = seq(0, 20, 1),)
axis(side = 2, at = seq(0, 20000, 1000),)
legend('topright',legend=c('More than','At least','Less than'),
       col=c(col1,col2,col3),
       pt.cex=2,pch=15,cex=1.2)

# Plot no more than, no less than and at most
hist(mt.neg.num,col=col1,breaks=20,xlim=c(0,15),ylim=c(0,800),xaxt="n",
     main='',ylab='',xlab='',yaxt='n')
hist(lt.neg.num,col=col2,add=T,breaks=20)
hist(am.num,col=col3,add=T,breaks=20)
axis(side = 1, at = seq(0, 15, 1),)
axis(side = 2, at = seq(0, 900, 100),)
legend('topright',legend=c('No more than','No less than','At most'),
       col=c(col1,col2,col3),
       pt.cex=2,pch=15,cex=1.2)

# Plot all numbers
hist(log.num,col=col1,breaks=40,xlim=c(0,20),xaxt="n",main='',ylab='',xlab='',yaxt='n')
axis(side = 1, at = seq(0, 20, 1))
axis(side = 2, at = seq(0, 5500000, 100000))

