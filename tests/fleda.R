###################################################
### chunk number 1: 
###################################################
require(FLEDA)
data(ple4)
data(ple4sex)
data(ple4.index)
data(ple4.indices)


###################################################
### chunk number 2: 
###################################################
apply(ple4sex@catch@.Data,3,summary)


###################################################
### chunk number 3: 
###################################################
summary(apply(ple4sex@catch@.Data,2,sum))


###################################################
### chunk number 4: 
###################################################
# fine tune xyplot
ttl <- list(label="Catch trends by sex for plaice in IV", cex=1)
yttl <- list(label=units(ple4sex@catch), cex=0.7)
xttl <- list(cex=0.7)
stripttl <- list(cex=0.7)
ax <- list(cex=0.7)
# plot
xyplot(data~year|unit, data=ple4sex@catch, type="l", main=ttl, ylab=yttl, xlab=xttl, par.strip.text=stripttl, scales=ax)


###################################################
### chunk number 5: 
###################################################

# fine tune xyplot
ttl <- list(label="Catch trends by sex for plaice in IV", cex=1)
yttl <- list(label=units(ple4sex@catch), cex=0.7)
xttl <- list(cex=0.7)
stripttl <- list(cex=0.7)
ax <- list(cex=0.7)
# plot
print(xyplot(data~year|unit, data=ple4sex@catch, type="l", main=ttl, ylab=yttl, xlab=xttl, par.strip.text=stripttl, scales=ax))


###################################################
### chunk number 6: 
###################################################
xyplot(data~year, data=ple4sex@catch, groups=unit, type="l", main="catch trends by sex for plaice in IV", ylab=units(ple4sex@catch))


###################################################
### chunk number 7: 
###################################################

print(xyplot(data~year, data=ple4sex@catch, groups=unit, type="l", main="catch trends by sex for plaice in IV", ylab=units(ple4sex@catch)))


###################################################
### chunk number 8: 
###################################################
# define the panel function to include a loess fit
pfun <- function(x,y, ...){
	panel.xyplot(x,y, type="l", lty=1, col=4, ...)
	panel.loess(x,y, span=0.5, lty=2, col=1)}
# fine tune 
ttl <- list(label="Catch trends for plaice in IV", cex=1)
yttl <- list(label=units(ple4@catch), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# legend
akey <- list(text=list(c("observed","loess(span=0.5)"), cex=0.7), border=T, lines=list(lty=c(1,2), col=c(4,1)))
# plot
xyplot(data~year, data=ple4@catch, panel=pfun, main=ttl, ylab=yttl, xlab=xttl, scales=ax, key=akey)


###################################################
### chunk number 9: 
###################################################

# define the panel function to include a loess fit
pfun <- function(x,y, ...){
	panel.xyplot(x,y, type="l", lty=1, col=4, ...)
	panel.loess(x,y, span=0.5, lty=2, col=1)}
# fine tune 
ttl <- list(label="Catch trends for plaice in IV", cex=1)
yttl <- list(label=units(ple4@catch), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# legend
akey <- list(text=list(c("observed","loess(span=0.5)"), cex=0.7), border=T, lines=list(lty=c(1,2), col=c(4,1)))
# plot
print(xyplot(data~year, data=ple4@catch, panel=pfun, main=ttl, ylab=yttl, xlab=xttl, scales=ax, key=akey))


###################################################
### chunk number 10: 
###################################################
# compute catch proportions at age
ple4sex.pay <- pay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.pay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=4)


###################################################
### chunk number 11: 
###################################################

# compute catch proportions at age
ple4sex.pay <- pay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.pay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=4))


###################################################
### chunk number 12: 
###################################################
# compute relative catch proportion at age
ple4sex.rpay <- rpay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Relative catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.rpay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5)


###################################################
### chunk number 13: 
###################################################

# compute relative catch proportion at age
ple4sex.rpay <- rpay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Relative catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.rpay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5))


###################################################
### chunk number 14: 
###################################################
# compute standardized catch proportion at age
ple4sex.spay <- spay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Standardized catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.spay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5)


###################################################
### chunk number 15: 
###################################################

# compute standardized catch proportion at age
ple4sex.spay <- spay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Standardized catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.spay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5))


###################################################
### chunk number 16: 
###################################################
# to a single index
mv0(index(ple4.index))
# or to a list of indices (FLIndices object)
data(ple4.indices)
lapply(ple4.indices, function(x) mv0(index(x)))


###################################################
### chunk number 17: 
###################################################
arr <- cor(index(ple4.index))
round(arr,2)


###################################################
### chunk number 18: 
###################################################
arr <- cor(index(ple4.indices[[1]]), index(ple4.indices[[2]]))
round(arr,2)


###################################################
### chunk number 19: 
###################################################
ttl <- list("Pairwise plot of age by year for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
splom(~data, data=index(ple4.index), panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3)


###################################################
### chunk number 20: 
###################################################

ttl <- list("Pairwise plot of age by year for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
print(splom(~data, data=index(ple4.index), panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3))


###################################################
### chunk number 21: 
###################################################
ttl <- list("Pairwise plot of age by cohort for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
flc <- FLCohort(index(ple4.index))
splom(~data, data=flc, panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3)


###################################################
### chunk number 22: 
###################################################

ttl <- list("Pairwise plot of age by cohort for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
flc <- FLCohort(index(ple4.index))
print(splom(~data, data=flc, panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3))


###################################################
### chunk number 23: 
###################################################
# let's extract the index slot from each FLIndex object in the FLIndices object.
lst <- lapply(ple4.indices, index)
# now a nice FLQuants
ple4.inds <- mcf(lst)
# scale
ple4.indsN01 <- lapply(ple4.inds, function(x){
                  arr <- apply(x@.Data, c(1,3,4,5), scale)
                  arr <- aperm(arr, c(2,1,3,4,5))
                  # small trick to fix an apply "feature"
                  dimnames(arr) <- dimnames(x)
                  x <- FLQuant(arr)
                })
 
ple4.indsN01 <- FLQuants(ple4.indsN01)
# stupid hack to correct names (fixed in version 2)
names(ple4.indsN01) <- names(lst)
# fine tune
ttl <- list("Standardized Surveys CPUE for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Standardized CPUE", cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
akey <- simpleKey(text=names(ple4.indsN01), points=F, lines=T, columns=2, cex=0.8)
# plot
xyplot(data~year|factor(age), data=ple4.indsN01, type="l", main=ttl, xlab=xttl, ylab=yttl, key=akey, striptext=stripttl, scales=ax, as.table=TRUE, layout=c(5,2,1))


###################################################
### chunk number 24: 
###################################################

# let's extract the index slot from each FLIndex object in the FLIndices object.
lst <- lapply(ple4.indices, index)
# now a nice FLQuants
ple4.inds <- mcf(lst)
# scale
ple4.indsN01 <- lapply(ple4.inds, function(x){
                  arr <- apply(x@.Data, c(1,3,4,5), scale)
                  arr <- aperm(arr, c(2,1,3,4,5))
                  # small trick to fix an apply "feature"
                  dimnames(arr) <- dimnames(x)
                  x <- FLQuant(arr)
                })
ple4.indsN01 <- FLQuants(ple4.indsN01)
# stupid hack to correct names (fixed in version 2)
names(ple4.indsN01) <- names(lst)
# fine tune
ttl <- list("Standardized Surveys CPUE for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Standardized CPUE", cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
akey <- simpleKey(text=names(ple4.indsN01), points=F, lines=T, columns=2, cex=0.8)
# plot
print(xyplot(data~year|factor(age), data=ple4.indsN01, type="l", main=ttl, xlab=xttl, ylab=yttl, key=akey, striptext=stripttl, scales=ax, as.table=TRUE, layout=c(5,2,1))
)


###################################################
### chunk number 25: 
###################################################
# fine tune plot
ttl <- list(label="Catch weight at age for Place in IV", cex=1)
yttl <- list(label=units(ple4sex@catch.wt), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
xyplot(data~year|unit, data=ple4sex@catch.wt, groups=age, type="l", lty=1:15, col=1, main=ttl, ylab=yttl, xlab=xttl, scales=ax)


###################################################
### chunk number 26: 
###################################################

ttl <- list(label="Catch weight at age for Place in IV", cex=1)
yttl <- list(label=units(ple4sex@catch.wt), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(xyplot(data~year|unit, data=ple4sex@catch.wt, groups=age, type="l", lty=1:15, col=1, main=ttl, ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 27: 
###################################################
# fine tune
ttl <- list(label="Maturity ogive by sex for plaice in IV", cex=1)
yttl <- list(label="%", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.7)
ax <- list(x=list(tick.number=7, cex=0.7), y=list(cex=0.7))
# legend
akey <- simpleKey(text=c("male", "female"), points=F, lines=T)
# plot
xyplot(data~age|as.factor(year), data=ple4sex@mat, type="l", groups=unit, key=akey, main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl)


###################################################
### chunk number 28: 
###################################################

ttl <- list(label="Maturity ogive by sex for plaice in IV", cex=1)
yttl <- list(label="%", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.7)
ax <- list(x=list(tick.number=7, cex=0.7), y=list(cex=0.7))
akey <- simpleKey(text=c("male", "female"), points=F, lines=T)
print(xyplot(data~age|as.factor(year), data=ple4sex@mat, type="l", groups=unit, key=akey, main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl))


###################################################
### chunk number 29: 
###################################################
# compute mature and immature biomass
ple4.bmass <- bmass(ple4)
# tune plot
ttl <- list(label="Trends in biomass for mature and immature plaice in IV", cex=1)
yttl <- list(label="relative biomass", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.8)
akey <- simpleKey(text=c("mature", "immature"), points=F, lines=T)
# plot
xyplot(data~year, data=ple4.bmass, type="l", main=ttl, key=akey, ylab=yttl, xlab=xttl, scales=ax)


###################################################
### chunk number 30: 
###################################################
ple4.bmass <- bmass(ple4)
ttl <- list(label="Trends in biomass for mature and immature plaice in IV", cex=1)
yttl <- list(label="relative biomass", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.8)
akey <- simpleKey(text=c("mature", "immature"), points=F, lines=T)
print(xyplot(data~year, data=ple4.bmass, type="l", main=ttl, key=akey, ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 31: 
###################################################
# compute logcc
ple4sex.cc <- logcc(ple4sex@catch.n)
# fine tune plot
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
ccplot(data~age|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1)


###################################################
### chunk number 32: 
###################################################

ple4sex.cc <- logcc(ple4sex@catch.n)
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(ccplot(data~age|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1))


###################################################
### chunk number 33: 
###################################################
# compute logcc
ple4sex.cc <- logcc(ple4sex@catch.n)
# fine tune
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
ccplot(data~year|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1)


###################################################
### chunk number 34: 
###################################################

ple4sex.cc <- logcc(ple4sex@catch.n)
# fine tune
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(ccplot(data~year|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1))


###################################################
### chunk number 35: 
###################################################
# compute Z
ple4z <- z(ple4@catch.n, agerng=3:6)
summary(ple4z)
t.test(ple4z)


###################################################
### chunk number 36: 
###################################################
# compute Z
ple4z <- z(ple4@catch.n, agerng=3:6)
# tune plot
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
# plot z by age along years
xyplot(data~year, data=ple4z@zy, type="l", main=ttl, ylab=yttl, xlab=xttl)


###################################################
### chunk number 37: 
###################################################

ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
print(xyplot(data~year, data=ple4z@zy, type="l", main=ttl, ylab=yttl, xlab=xttl))


###################################################
### chunk number 38: 
###################################################
# plot z by age along ages
ax <- list(x=list(at=c(3:6)))
xyplot(data~age, data=ple4z@za, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax)


###################################################
### chunk number 39: 
###################################################

ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
ax <- list(x=list(at=c(3:6)))
print(xyplot(data~age, data=ple4z@za, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 40: 
###################################################
# plot z by cohort along years
xyplot(data~cohort, data=ple4z@zc, type="l", main=ttl, ylab=yttl, xlab=xttl)


###################################################
### chunk number 41: 
###################################################

ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
print(xyplot(data~cohort, data=ple4z@zc, type="l", main=ttl, ylab=yttl, xlab=xttl))


