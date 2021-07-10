
library(fda)

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
                 crossprod(thawbasismat,thawdata[,1:num.years]))
thawfd = fd(thawcoef, thawbasis,
            list("Day", "Year", "Temperature (deg C)"))
plot(thawfd, lty=1, lwd=2, col=1)

plotfit.fd(thawdata[,1], daytime, thawfd[1], lty=1, lwd=2) # Only for 1961

