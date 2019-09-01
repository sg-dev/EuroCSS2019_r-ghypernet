## Task: Tutorial on ghypernets package for gHypEG regressions on mulit-edge networks
## Authors: Laurence Brandenberger and Giona Casiraghi, Chair of Systems Design, ETH Zurich
## Date: September 2, 2019

################################################################################
################################################################################
################################################################################

## load relevant packages
library(ghypernet)
library(networkRegression)
library(texreg) # for regression tables
library(ggplot2) 
library(GGally) #for network plots using ggplot2

## Overview
# Part 1: Data preparation
# Part 2: Running gHypEG regressions
# Part 3: Model assessment, simulations and goodness-of-fit

## load tutorial data set
data("swissParliament_network", package = "ghypernet")
#load("../data/swissParliament_network.RData")
# Data: excerpt of over 200 MPs of the Swiss National Council.

## cospons_mat: contains the adjacency matrix of 163 x 163 MPs.
  #It contains the number of times one MP (rows) supports the submitted proposals of
  #another MP (columns).
## dt: contains different attributes of the 163 MPs, such as their names, 
  #affiliation (variable: *parlGroup*), the Canton (or state) they represent
  #their party affiliation (variable: *party*), their parliamentary group
  #(variable: *canton*), their gender  (variable: *gender*)
  #and date of birth  (variable: *birthdate*).
## dtcommittee: a list of committees each MP was part of during their stay in 
  #parliament
## onlinesim_mat: a similarity matrix of how similar two MPs are in their online
  #social media presence (shared supportees).

################################################################################
## Part 1: Data preparation
################################################################################

#####################
## Network data manipulations
#####################

# moving from an adjacency matrix to an edgelist
el <- adj2el(cospons_mat, directed = TRUE)

# moving from an edgelist to an adjacancy matrix
nodes <- colnames(cospons_mat)
adj_mat <- el2adj(el, nodes = nodes)

#####################
## Compiling nodal attribute data
#####################

# Your attribute data and your adjacancy matrix have to match
identical(rownames(cospons_mat), dt$idMP)

# Generally, they don't match from the start
dt_unsorted <- dt[order(dt$firstName),]
identical(rownames(cospons_mat), dt_unsorted$idMP)

# If they don't match: create a new data frame, and join in the attributes
dtsorted <- data.frame(idMP = rownames(cospons_mat)) 
dtsorted <- dplyr::left_join(dtsorted, dt_unsorted, by = "idMP") 
identical(dt$idMP, dtsorted$idMP)

#####################
## Calculating reciprocity
#####################

# use the reciprocity_stat for weighted reciprocity stats
recip_cospons <- reciprocity_stat(cospons_mat)
recip_cospons[1:5, 1:3]

# you can also feed in an edgelist
recip_cospons_fromel <- reciprocity_stat(graph = el, nodes = rownames(cospons_mat))
recip_cospons_fromel[1:5, 1:3]
identical(recip_cospons, recip_cospons_fromel)

#####################
## Calculating triadic closure
#####################

# use the sharedPartner_stat for (un-)weighted closure stats
shp_cospons_unweighted <- sharedPartner_stat(cospons_mat, directed = TRUE, weighted = FALSE)
shp_cospons_unweighted[1:5, 1:3]
#Note: the unweighted shared partner statistic is very unreliable for dense multi-edge graphs

# the weighted matrix..
shp_cospons_weighted <- sharedPartner_stat(cospons_mat, directed = TRUE, 
                                           zero_values = .1)
shp_cospons_weighted[1:5, 1:3]

# for directed networks, you can calculate incoming and outgoing triadic closure
shp_cospons_incoming <- sharedPartner_stat(cospons_mat, directed = TRUE,
                                           triad.type = 'directed.incoming',
                                           zero_values = .1)
shp_cospons_incoming[1:5, 1:3]
# incoming: two nodes have the same supporter

shp_cospons_outgoing <- sharedPartner_stat(cospons_mat, directed = TRUE,
                                           triad.type = 'directed.outgoing', 
                                           zero_values = .1)
shp_cospons_outgoing[1:5, 1:3]
# outgoing: two nodes support the same third node

#####################
## Calculating homophily stats
#####################

## Cantonal homophily
canton_homophilymat <- homophily_stat(dt$canton, type = 'categorical', 
                                      nodes = dt$idMP)
canton_homophilymat[1:5, 1:3]

## Cantonal homophily: you can do it for one category only..
canton_BE_homophilymat <- homophily_stat(dt$canton, type = 'categorical', 
                                         nodes = dt$idMP, these.categories.only = 'Bern')
## Cantonal homophily: ..or for multiple categories
canton_BEZH_homophilymat <- homophily_stat(dt$canton, type = 'categorical', 
                                           nodes = dt$idMP, 
                                           these.categories.only = c('Bern', 'Zuerich'))

## Party homophily
table(dt$party)
party_homophilymat <- homophily_stat(dt$party, type = 'categorical', nodes = dt$idMP)

## Parliamentary group homophily
table(as.character(dt$parlGroup))
parlgroup_homophilymat <- homophily_stat(dt$parlGroup, type = 'categorical', nodes = dt$idMP)

## Gender homophily
table(dt$gender) 
gender_homophilymat <- homophily_stat(dt$gender, type = 'categorical', nodes = dt$idMP)

## Age difference (absolute difference)
dt$age <- 2019 - as.numeric(format(as.Date(dt$birthdate, format = '%d.%m.%Y'), "%Y"))
age_absdiffmat <- homophily_stat(dt$age,  type = 'absdiff', nodes = dt$idMP)
age_absdiffmat[1:5, 1:3]

#####################
## Create custom covariates
#####################

## Example: committe membership data
head(dtcommittee)

## You can create your own matrix out of it (any way you want, I usually go the for-loop way)
# a) check rownames
identical(as.character(dtcommittee$idMP), rownames(cospons_mat))
# b) create empty matrix
shared_committee <- matrix(0, nrow = nrow(cospons_mat), ncol = ncol(cospons_mat))
rownames(shared_committee) <- rownames(cospons_mat)
colnames(shared_committee) <- colnames(cospons_mat)
# c) fill up the matrix (the slow and steady way :))
for(i in 1:nrow(shared_committee)){
  for(j in 1:ncol(shared_committee)){
    committees_i <- unlist(strsplit(as.character(dtcommittee$committee_names[i]), ";"))
    committees_j <- unlist(strsplit(as.character(dtcommittee$committee_names[j]), ";"))
    shared_committee[i, j] <- length(intersect(committees_i, committees_j))
  }
}
# d) replace zero values
shared_committee[shared_committee == 0] <- 0.1 # replace zero-values
# e) check the code
shared_committee[1:5, 1:3]

################################################################################
## Part 2: Running gHypEG regressions
################################################################################

#####################
## The general set up of the nrm-function for gHypEG regressions
#####################

fit <- nrm(adj = cospons_mat, w = list(reciprocity = recip_cospons), 
           directed = TRUE, selfloops = FALSE, regular = FALSE)

#####################
## Running regressions
#####################

## estimate the effect of canton homophily
nfit1 <- nrm(adj = cospons_mat, 
             w = list(same_canton = canton_homophilymat), 
             directed = TRUE)
summary(nfit1)
# Interpretation for dummy variables: 
# Coefficient = 0.0906920; 10^0.09 = 1.23; The odds of MP i co-sponsoring the bill of MP j increases by a factor of 1.23
(10^(0.09))/(1^(0.09)) # = 1.23

## to speed things up, set the init value
nfit1 <- nrm(adj = cospons_mat, 
             w = list(canton = canton_homophilymat), 
             directed = TRUE,
             init = c(0.09))
summary(nfit1)

## fill it up some more
nfit2 <- nrm(adj = cospons_mat, 
             w = list(party = party_homophilymat,
                      canton = canton_homophilymat, 
                      gender = gender_homophilymat,
                      age = age_absdiffmat), 
             directed = TRUE, 
             init = c(.5, .1, 0, 0))
summary(nfit2)
# Party homophily: 10^(0.83) = 6.76083; odds of co-sponsorship increases by facotr of 6.7 if both are from the same party
# Cantonal homophily: 10^(0.13) = 1.35; 
# Gender homophily: 10^(0.095) = 1.24;
# Age difference is a bit different:
  # from zero to 1 year difference: (.1^(-0.0526086))/(1^(-0.0526086)) = 1.13
  # from 1 year to 10 year difference: (1^(-0.0526086))/(10^(-0.0526086)) = 1.13
  # from 1 year to 20 year difference: (1^(-0.0526086))/(20^(-0.0526086)) = 1.17

## 
nfit3 <- nrm(adj = cospons_mat, 
             w = list(reciprocity = recip_cospons ), 
             directed = TRUE)
summary(nfit3)
# Reciprocity: from zero to 1: (1^(0.4442337))/(.1^(0.4442337)) = 2.78

##
nfit4 <- nrm(adj = cospons_mat, 
             w = list(reciprocity = recip_cospons,
                      sharedpartner_in = shp_cospons_incoming
                      #sharedpartner_out = shp_cospons_outgoing 
                      ), 
             directed = TRUE, 
             init = c(.4, 0))
summary(nfit4)
# Shared partners: from zero to 1: (1^(-0.0589001))/(.1^(-0.0589001)) = .87

## Additional matrices
nfit5 <- nrm(adj = cospons_mat, 
             w = list(committee = shared_committee,
                      online_similarity = onlinesim_mat ), 
             directed = TRUE, 
             init = c(.1, 0.3))
summary(nfit5)
# Committe membership: from zero to 1: (1^(0.0383108))/(.1^(0.0383108)) = 1.092222
# Online similarity: from zero to 0.5: (.5^(0.4108739))/(0.001^(0.4108739)) = 12.85

## Put all together
nfit6 <- nrm(adj = cospons_mat, 
             w = list(reciprocity = recip_cospons,
                      sharedpartner_in = shp_cospons_incoming,
                      #sharedpartner_out = shp_cospons_outgoing, 
                      party = party_homophilymat,
                      canton = canton_homophilymat, 
                      gender = gender_homophilymat,
                      age = age_absdiffmat,
                      committee = shared_committee,
                      online_similarity = onlinesim_mat ), 
             directed = TRUE, 
             init = c(.1, 0, .4, 0.1, 0, 0, 0.03, 0.1))
summary(nfit6)
# Notice how online similarity changes?

## Most important predictor: 
nfit7 <- nrm(adj = cospons_mat, 
             w = list(party = party_homophilymat), 
             directed = TRUE, 
             init = c(.4))
summary(nfit7)

#####################
## Create regression tables
#####################

# use the extract()-function to make this available:
extract.nrm.cluster <- function(model,sumsum){
  # calculate SE, tvalues and pvalues
  coeffic <- as.numeric(model$coef)
  stderr <- (model$confint[,2] - model$confint[,1])/5.15
  tvalues = coeffic/stderr
  pval <- exp(-0.717*tvalues - 0.416*tvalues^2)
  
  # then create and return a texreg object (replace NULL with actual values):
  tr <- createTexreg(
    coef.names = names(model$coef),    # character vector of coefficient labels
    coef = coeffic,          # numeric vector with coefficients
    se = stderr,            # numeric vector with standard error values
    pvalues = pval,       # numeric vector with p-values
    gof.names = c("AIC", "McFadden $pseudo-R^2$"),     # character vector with goodness-of-fit labels
    gof = c(model$AIC, model$R2)           # numeric vector of goodness-of-fit statistics
    #gof.decimal = NULL    # logical vector: GOF statistic has decimal points?
  )
  return(tr)
}
setMethod("extract", signature = className("nrm", "ghype"), 
          definition = extract.nrm.cluster)

##
screenreg(list(nfit6, nfit1, nfit2, nfit3, nfit4, nfit5, nfit7))
# Notice AIC
# Notice reciprocity and party homophily => why do they both load so strongly on each other? Which dominates and why?


################################################################################
## Part 3: Model assessment, simulations and goodness-of-fit
################################################################################

#####################
## Predicted probabilities
#####################

## Let's look at some predicted probabilities
nfit6omega <- data.frame(omega = nfit6$omega, 
                         cosponsfull = c(cospons_mat), 
                         age_absdiff = c(age_absdiffmat), 
                         sameparty = c(party_homophilymat))
nfit6omega[nfit6omega == 0] <- NA
nfit6omega <- na.omit(nfit6omega)

##
ggplot(nfit6omega, aes(x = age_absdiff, y = omega, color = factor(sameparty)))+
  geom_point(alpha = .1) +
  geom_smooth() + theme(legend.position = 'bottom') + 
  scale_color_manual("", values = c('#E41A1C', '#377EB8'), labels = c('Between parties', 'Within party'))+
  xlab("Age difference") + ylab("Tie propensities")+
  ggtitle('Model (2): Marginal effects of age difference')

## Can be done with any other variable -- try it with reciprocity
nfit6omega <- data.frame(omega = nfit6$omega, 
                         cosponsfull = c(cospons_mat), 
                         age_absdiff = c(age_absdiffmat), 
                         sameparty = c(party_homophilymat), 
                         reciprocity = c(recip_cospons))
nfit6omega[nfit6omega == 0] <- NA
nfit6omega <- na.omit(nfit6omega)
ggplot(nfit6omega, aes(x = reciprocity, y = omega, color = factor(sameparty)))+
  geom_point(alpha = .1) +
  geom_smooth() + theme(legend.position = 'bottom') + 
  scale_color_manual("", values = c('#E41A1C', '#377EB8'), labels = c('Between parties', 'Within party'))+
  xlab("Reciprocity (weighted)") + ylab("Tie propensities")+
  ggtitle('Model (2): Marginal effects of reciprocity')

#####################
## Simulate new networks
#####################

simnw <- rghype(nsamples = 1, model = nfit6, seed = 1253)
ggnet2(simnw, label = FALSE,
       color = dt$parlGroup, 
       size = "degree", size.min = 1)+
  scale_color_manual("", values = c('orange', 'yellow', 'blue', 'green', 'grey',
                                    'darkblue', 'red', 'darkgreen', 'purple'))+
  theme(legend.position = 'bottom') +
  guides(size = FALSE)

#####################
## Goodness-of-fit
#####################

## tbd. we're working on this.
