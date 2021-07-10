
library(fda)
library(refund)

# CHAPTER 4
MtlDaily = MontrealTemp # Pull up Montreal temperature data 1961-1994 
thawdata = t(MtlDaily[,16:47]) # Create thawdata  between Jan01 and Feb19, 1961-1994; 32(days) by 34(years) matrix 
daytime = ((16:47)+0.5)
par(cex=1.2)
plot(daytime, apply(thawdata,1,mean), "b", lwd=2,
     xlab="Day", ylab="Temperature (deg C)")


thawbasis = create.bspline.basis(c(16,48),7)
thawbasismat = eval.basis(daytime, thawbasis)

num.years = nrow(thawdata) # how many years
thawcoef = solve(crossprod(thawbasismat),
                 crossprod(thawbasismat,thawdata[,1:num.years]) )
thawcoef_alternative = lsfit(x = thawbasismat, y = thawdata[,1:num.years], intercept=FALSE)$coef # NOTE: 'thawcoef' and
# 'thawcoef_alternative' are exactly the same.

thawfd = fd(thawcoef, thawbasis,
            list("Day", "Year", "Temperature (deg C)"))
plot(thawfd, lty=1, lwd=2, col=1)

plotfit.fd(thawdata[,1], daytime, thawfd[1], lty=1, lwd=2) # Only for 1961


# CHAPTER 5
heightmat = growth$hgtm # heights of 39 boys from 1yr to 18yrs
age = growth$age
heightbasis12 = create.bspline.basis(c(1,18), 12, 6) # 12 basis functions, 6 interior knots
basismat = eval.basis(age, heightbasis12)
heightcoef = lsfit(x =basismat, y = heightmat, intercept=FALSE)$coef
heightList = smooth.basis(argvals= age, y = heightmat, fdParobj = heightbasis12)
heightfd = heightList$fd
height.df = heightList$df
height.gcv = heightList$gcv

# Same thing with the lines above 
age = growth$age
heightbasismat = eval.basis(age, heightbasis12)
y2cMap = solve(crossprod(heightbasismat),
               t(heightbasismat))













