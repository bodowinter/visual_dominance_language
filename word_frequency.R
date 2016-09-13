## Bodo Winter
## September 17, 2015; New approach October 15, 2015
## Based on earlier analysis for PhD thesis of Ch. 3, see:
## https://github.com/bodowinter/phd_thesis
## Cleaned up for resubmission September, 13, 2016

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)
library(MASS)
library(pscl)

## Define path to parent directory:

mainPath <- '/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/teeniematlock/Desktop/research/senses_sensory_modalities/viberg/new_analysis/'
setwd(mainPath)

## Load in modality norms:

setwd(file.path(mainPath, 'data'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2016_verb_norms.csv')

## Load in other datasets used in this particular analysis:

verbs <- read.csv('COCA_verb_frequencies.csv')
nouns <- read.csv('COCA_noun_frequencies.csv')
adjs <- read.csv('COCA_adj_frequencies.csv')

## Merge COCA frequency data into l, d and n:

l <- cbind(l, adjs[match(l$Word, adjs$Word), -1])
n <- cbind(n, nouns[match(n$Word, nouns$Word), -1])
v <- cbind(v, verbs[match(v$Word, verbs$Word), -1])

## For how many was there no frequency data?

sum(is.na(l$Freq))
sum(is.na(n$Freq))
sum(is.na(v$Freq))

## Make those that are NA into 0 (since it is not attested in the corpus):

l[is.na(l$Freq), c('Freq', 'CD')] <- 0
n[is.na(n$Freq), c('Freq', 'CD')] <- 0
v[is.na(v$Freq), c('Freq', 'CD')] <- 0

## Create a data frame from the random subset:

v <- filter(v, RandomSet == 'yes')
# (you can check the analysis with the full set
# and they won't change in any substantively meaningful fashion)



##------------------------------------------------------------------
## Simple average frequency analysis:
##------------------------------------------------------------------

## Take aggregate:

xagr <- rbind(dplyr::select(l, Word, DominantModality,
				VisualStrengthMean:OlfactoryStrengthMean,
				ModalityExclusivity, Freq:CD),
			dplyr::select(n, Word, DominantModality,
				VisualStrengthMean:OlfactoryStrengthMean,
				ModalityExclusivity, Freq:CD),
			dplyr::select(v, Word, DominantModality,
				VisualStrengthMean:OlfactoryStrengthMean,
				ModalityExclusivity, Freq:CD))

## Order modality vector:

xagr$DominantModality <- factor(xagr$DominantModality,
	levels = c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory'))

## Add POS column to both datasets:

xagr$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)), rep('verb', nrow(v)))
# (here it does not matter whether words may also be of other POS since
# importantly, each norming study was ONLY nouns, ONLY verbs etc. ...
# so these are the lexical categories that the participants considered)

## Center all relevant variables in dataset because there will be interactions:

xagr <- mutate(xagr,
	VisualStrengthMean_c = VisualStrengthMean - mean(VisualStrengthMean),
	HapticStrengthMean_c = HapticStrengthMean - mean(HapticStrengthMean),
	AuditoryStrengthMean_c = AuditoryStrengthMean - mean(AuditoryStrengthMean),
	GustatoryStrengthMean_c = GustatoryStrengthMean - mean(GustatoryStrengthMean),
	OlfactoryStrengthMean_c = OlfactoryStrengthMean - mean(OlfactoryStrengthMean),
	ModalityExclusivity_c = ModalityExclusivity - mean(ModalityExclusivity))

## Make a model for each modality:

summary(xmdl.vis <- glm.nb(Freq ~ VisualStrengthMean_c *
	ModalityExclusivity_c, xagr))
summary(xmdl.hap <- glm.nb(Freq ~ HapticStrengthMean_c *
	ModalityExclusivity_c, xagr))
summary(xmdl.aud <- glm.nb(Freq ~ AuditoryStrengthMean_c *
	ModalityExclusivity_c, xagr))
summary(xmdl.gus <- glm.nb(Freq ~ GustatoryStrengthMean_c *
	ModalityExclusivity_c, xagr))
summary(xmdl.olf <- glm.nb(Freq ~ OlfactoryStrengthMean_c *
	ModalityExclusivity_c, xagr))

## Check whether negative binomial model is needed (due to overdispersion):
odTest(xmdl.vis)	# yep

## To compute example values:

max_vis <- 5 - mean(xagr$VisualStrengthMean)		# coz of centering in model
min_vis <- -1 * mean(xagr$VisualStrengthMean)
newpred <- data.frame(VisualStrengthMean_c = c(min_vis, max_vis),
	ModalityExclusivity_c = 0)
newpred <- cbind(newpred,
	as.data.frame(predict(xmdl.vis, newpred, se.fit = T)[1:2]))
exp(newpred[1, ]$fit)
exp(newpred[1, ]$fit - 1.96 * newpred[1, ]$se.fit)
exp(newpred[1, ]$fit + 1.96 * newpred[1, ]$se.fit)
exp(newpred[2, ]$fit)
exp(newpred[2, ]$fit - 1.96 * newpred[2, ]$se.fit)
exp(newpred[2, ]$fit + 1.96 * newpred[2, ]$se.fit)



##------------------------------------------------------------------
## Categrical approach (not reported in paper):
##------------------------------------------------------------------

## Construct models with dominant modality classification:

summary(xmdl.nb <- glm.nb(Freq ~ DominantModality * POS, xagr))
summary(xmdl.noint <- glm.nb(Freq ~ DominantModality + POS, xagr))
summary(xmdl.nofreq <- glm.nb(Freq ~ 1 + POS, xagr))

## Likelihood ratio tests:

anova(xmdl.noint, xmdl.nb, test = 'Chisq')		# sig.
anova(xmdl.nofreq, xmdl.noint, test = 'Chisq')	# sig.



##------------------------------------------------------------------
## Simple cumulative frequencies:
##------------------------------------------------------------------

## Take means:

lfreqs <- aggregate(Freq ~ DominantModality, xagr[xagr$POS == 'adj', ], sum)[,-1]
nfreqs <- aggregate(Freq ~ DominantModality, xagr[xagr$POS == 'noun', ], sum)[,-1]
vfreqs <- aggregate(Freq ~ DominantModality, xagr[xagr$POS == 'verb', ], sum)[,-1]
vfreqs <- c(vfreqs, 0)

## Make a table out of this:

allfreqs <- c(lfreqs, nfreqs, vfreqs)
allfreqs <- round(allfreqs, -3) / 1000
allfreqs <- paste0(allfreqs, 'k')
M <- matrix(allfreqs, nrow = 3, byrow = T)

## Make the table presentable:

rownames(M) <- c('Adjectives', 'Nouns', 'Verbs')
colnames(M) <- c('Vision', 'Hearing', 'Touch', 'Taste', 'Smell')

## Calculate random draw frequency:

allfreqs <- c(lfreqs, nfreqs, vfreqs)
M <- matrix(allfreqs, nrow = 3, byrow = T)
Msums <- colSums(M)
round(Msums / sum(Msums), 2)
apply(M, 2, sum)		# cumulative frequencies across POS



##------------------------------------------------------------------
## Diachronic variation (Google NGram)
##------------------------------------------------------------------
# data collected using the ngramr package

## Load in Google data:

goo <- read.csv('google_ngram.csv')

## Add dominant modality information:

goo$Modality <- l[match(goo$Word, l$Word),]$DominantModality

## Compute averages:

goo_agr <- summarise(group_by(goo, Year, Modality),
	FreqMean = mean(Frequency), FreqSD = sd(Frequency))

## Create a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(1700, 2060), ylim = c(0, 0.00004))
for (i in 1:5) {
	this_modality <- unique(goo_agr$Modality)[i]
	with(goo_agr[goo_agr$Modality == this_modality,],
		points(Year, FreqMean, type = 'l', lwd = 2, col = 1)	# change for color
		)
	}
axis(side = 1, at = seq(1700, 2000, 100), lwd = 2, font = 2, cex.axis = 1.5)
axis(side = 2, at = seq(0, 0.00004, length.out = 5),
	labels = paste(0:4, 'e-05', sep = ''),
	lwd = 2, font = 2, cex.axis = 1.25, las = 2)
mtext(side = 1, text = 'Year',
	line = 3, font = 2, cex = 1.9)
mtext(side = 2, text = 'Relative frequency', line = 4.6, font = 2, cex = 1.8)
axis(side = 1, at = 1:7, lwd = 2, font = 2, cex.axis = 1.5)
text(x = 1900, y = 0.0000375, labels = 'Visual', font = 2, cex = 1.5)
text(x = 1900, y = 0.0000165, labels = 'Tactile', font = 2, cex = 1.5)
text(x = 1900, y = 0.0000085, labels = 'Olfactory', font = 2, cex = 1.5)
text(x = 2040, y = 0.000004, labels = 'Auditory', font = 2, cex = 1.15)
text(x = 2040, y = 0.000002, labels = 'Gustatory', font = 2, cex = 1.15)


