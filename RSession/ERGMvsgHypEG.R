library(ghypernet)
library(ergm.count)
library(igraphdata)
library(igraph)

####### karate club

data("adj_karate")
data("vertexlabels")

adj <- adj_karate
block <- homophily_stat(vertexlabels, nodes = rownames(adj))

triangles_ij <- sharedPartner_stat(graph = adj, directed = FALSE)

#ghypeg

(nr.mnoddeg <- nrm(w = list(tri=triangles_ij+1, block=block), 
                   adj = as.matrix(adj), directed = F, selfloops = F, regular = TRUE))
(nr.mddeg <- nrm(w = list(tri=triangles_ij+1, block=block), 
                 adj = as.matrix(adj), directed = F, selfloops = F, regular = FALSE))

AIC(regularm(adj, F,F))
AIC(nr.mnoddeg)

AIC(scm(adj, F,F))
AIC(nr.mddeg)

AIC(ghype(adj, F, F))

(nulldev <- loglratio(mod0 = ghype(adj,F,F), mod1 = scm(adj,F,F)))
(resdev <- loglratio(mod0 = ghype(adj,F,F), mod1 = nr.mddeg))

#ergm
data("karate")
zach <- network(adj, directed = FALSE)
zach %e% "contexts" = adj
zach %v% "wdeg" = apply(adj,1,sum)
zach %v% "faction" = V(karate)$Faction

zach.null <- ergm(zach~nonzero+sum,
                  response="contexts", reference=~Poisson,
                  control=control.ergm(MCMLE.trustregion=1000))
summary(zach.null)
logLik(zach.null)

zach.fit1 <- ergm(zach~nonzero+sum+edgecov(triangles_ij)+nodematch('faction'),
                  response="contexts", reference=~Poisson,
                  control=control.ergm(MCMLE.trustregion=1000))
summary(zach.fit1)

mcmc.diagnostics(zach.fit1)

zach.fit2 <- ergm(zach~nonzero+sum+edgecov(triangles_ij)+nodecov("wdeg")+nodematch('faction'),
                  response="contexts", reference=~Poisson,
                  control=control.ergm(MCMLE.trustregion=1000))
logLik(zach.fit2)
summary(zach.fit2)

mcmc.diagnostics(zach.fit2)

############# sociopatterns

data("highschool.predictors")
load('highschoolData.RData')
adj <- contacts.adj

# ghype

triangles_ij <- sharedPartner_stat(adj, directed = FALSE)
(nr.mnoddegnotri <- nrm(w = list(block=highschool.predictors[['class']]*10),
                        adj = as.matrix(adj), directed = F, selfloops = F, regular = TRUE))
(nr.mddegnotri <- nrm(w = list(block=highschool.predictors[['class']]*10),
                      adj = as.matrix(adj), directed = F, selfloops = F, regular = FALSE))
(nr.mnoddeg <- nrm(w = list(tri=triangles_ij+1, block=highschool.predictors[['class']]*10),
                   adj = as.matrix(adj), directed = F, selfloops = F, regular = TRUE))
(nr.mddeg <- nrm(w = list(tri=triangles_ij+1, block=highschool.predictors[['class']]*10),
                 adj = as.matrix(adj), directed = F, selfloops = F, regular = FALSE))

AIC(regularm(adj, F,F))
AIC(scm(adj, F,F))
AIC(nr.mnoddegnotri)
AIC(nr.mddegnotri)
AIC(nr.mnoddeg)
AIC(nr.mddeg)

# ergm

school <- network(adj, directed = FALSE)
school %e% "contexts" = adj
school %v% "wdeg" = apply(adj,1,sum)
school %v% "class" = metadata$X2

block_ergm <- highschool.predictors[['class']]
block_ergm[block_ergm==.1] <- 0
school.fit1 <- ergm(school~nonzero+sum+edgecov(triangles_ij)+nodematch('class'), #+edgecov(block_ergm),
                    response="contexts", reference=~Poisson,
                    control=control.ergm(MCMLE.trustregion=1000)) #, init=coef(school.fit1)))
summary(school.fit1)

mcmc.diagnostics(school.fit1)

school.fit2 <- ergm(school~nonzero+sum+edgecov(triangles_ij)+nodecov("wdeg")+nodematch('class'), #+edgecov(block_ergm),
                    response="contexts", reference=~Poisson,
                    control=control.ergm(MCMLE.trustregion=1000))
summary(school.fit2)
stargazer::stargazer(school.fit2, title='ERGM Results')

mcmc.diagnostics(school.fit2)