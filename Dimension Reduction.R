# ------ Principal Component Analysis (PCA) ------
hep <- read.csv("C:/Users/Fiona/Desktop/Business Analytics and Decision Sciences/Forecasting and Business Analytics/FABA-L4-Notes/hep88.csv", header=TRUE)
hep$hurdles <- max(hep$hurdles) -  hep$hurdles
hep$run200m <- max(hep$run200m) -  hep$run200m
hep$run800m <- max(hep$run800m) - hep$run800m

# cor matrix - rounded to 3DP
round(cor(hep[2:8]), 3)						

# get corrgram plots
#install.packages("corrgram")
library("corrgram")
corrgram(hep[2:8], main="Corrgram of 1998 heptathlon data")
corrgram(hep[2:8], order=TRUE, upper.panel=panel.pie, main="Corrgram with Pies ordered by PCA")

# pairs plot
pairs(hep[2:8])							      

# PCA & scale to unit variance
heppca <- prcomp(hep[2:8], scale = TRUE)
summary(heppca)
heppca

# get eiganvalues using SD2
heppca$sdev ^ 2						

# scree plot (barplot)
plot(heppca)							

# normal scree plot (line)
plot(heppca, type="line")					

# PCA biplot
biplot(heppca)							

# Get scores
heppca$x							

# ------ PCA Rotation ------
# using the principal functiuon in the psych package
#install.packages("psych")
#install.packages("GPArotation")
library(psych)
library(GPArotation)
summary(prcomp(hep[2:8], scale = TRUE))
heppca2 <- principal(hep[2:8], nfactors = 2, rotate="none")
heppca2	# see the output is the same as usuing the prcomp function
# h2 = the amount of variance in each variable explained by the components.
# u2 = component uniqueness, i.e. the amount of variance not accounted for by the components.

# get biplot
biplot(heppca2)

# Using varimax to get components
principal(hep[2:8], nfactors = 2, rotate="varimax")	
biplot(principal(hep[2:8], nfactors = 2, rotate="varimax"))

# Using promax to get components
principal(hep[2:8], nfactors = 2, rotate="promax")	
biplot(principal(hep[2:8], nfactors = 2, rotate="promax"))

# Get scores
heppca2$scores					

# ------ Exploratory Factor Analysis ------
# show covariance data
Harman74.cor 						

# convert covariance to correlations
correlations <- cov2cor(Harman74.cor$cov)		
correlations

# Round to 3DP - still too difficult to read!
round(correlations, 3)					
library(corrgram) # Could also try plotcorr() in ellipse package?
corrgram(correlations)
fa.parallel(correlations, n.obs=145, fa="both", n.iter=100)

# FA with 4 factors using maximum likelihood
fa(correlations, nfactors=4, rotate="none", fm="ml")     

# with varimax rotation
fa(correlations, nfactors=4, rotate="varimax", fm="ml")	

# with promax roation
fa(correlations, nfactors=4, rotate="promax", fm="ml")	

