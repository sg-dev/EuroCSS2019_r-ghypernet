library(ghypernet)
library(igraph)

load('highschoolData.RData')

adj <-
  get.adjacency(graph_from_data_frame(el_contacts[, 2:3], directed = FALSE), sparse = FALSE)
fb <-
  get.adjacency(graph_from_data_frame(el_fb, directed = FALSE),
                sparse = FALSE,
                attr = 'X3')
fr <-
  get.adjacency(graph_from_data_frame(el_fr, directed = FALSE), sparse = FALSE)

samecat <- function(v, w)
  ifelse(v == w & v != 'Unknown', 10, 1)
samecatM <- function(v, w)
  ifelse(v == w & v == 'M', 10, 1)
samecatF <- function(v, w)
  ifelse(v == w & v == 'F', 10, 1)
samecatNA <-
  function(v, w)
    ifelse(v == 'Unknown' | w == 'Unknown', 10, 1)
metadata <- metadata[-which(!(metadata$X1 %in% rownames(adj))), ]
maporder <- function(id, v)
  which(id == v)
metadata <-
  metadata[Vectorize(FUN = maporder, vectorize.args = 'id')(id = rownames(adj), metadata$X1), ]

genderM_adj <-
  Vectorize(FUN = Vectorize(FUN = samecatM, vectorize.args = 'v'),
            vectorize.args = 'w')(metadata$X3, metadata$X3)
genderF_adj <-
  Vectorize(FUN = Vectorize(FUN = samecatF, vectorize.args = 'v'),
            vectorize.args = 'w')(metadata$X3, metadata$X3)
genderNA_adj <-
  Vectorize(FUN = Vectorize(FUN = samecatNA, vectorize.args = 'v'),
            vectorize.args = 'w')(metadata$X3, metadata$X3)

genderHomo <-
  Vectorize(FUN = Vectorize(FUN = samecat, vectorize.args = 'v'),
            vectorize.args = 'w')(metadata$X3, metadata$X3)

#### gender

homo.m <-
  nrm(
    w = list(Ghomo = genderHomo),
    adj = adj,
    directed = FALSE,
    selfloops = FALSE
  )
summary(homo.m)

gender <- list(M = genderM_adj, 'F' = genderF_adj, U = genderNA_adj)

gender.m <-
  nrm(
    w = gender,
    adj = adj,
    directed = FALSE,
    selfloops = FALSE,
    init = c(0.32, 0.35, 0.1)
  )
summary(gender.m)

AIC(homo.m)
AIC(gender.m)
AIC(homo.m) - AIC(gender.m)

gender <- list(gender = genderHomo)

#### class

sameclass <- function(v, w)
  ifelse((v == w), 10, 1)
sameclassv <-
  Vectorize(FUN = Vectorize(FUN = sameclass, vectorize.args = 'v'),
            vectorize.args = 'w')

class <- list(class = sameclassv(metadata$X2, metadata$X2))

(class.m <-
    nrm(
      w = class,
      adj = adj,
      directed = FALSE,
      selfloops = FALSE
    ))

##### topic


topics <- substr(metadata$X2, start = 1, stop = 2)

sameclass <- function(v, w)
  ifelse((v == w), 10, 1)
sameclassv <-
  Vectorize(FUN = Vectorize(FUN = sameclass, vectorize.args = 'v'),
            vectorize.args = 'w')

topic <- list(topic = sameclassv(topics, topics))

(topic.m <-
    nrm(
      w = topic,
      adj = adj,
      directed = FALSE,
      selfloops = FALSE
    ))

##### topic class
(tc.m <-
    nrm(
      w = c(class, topic),
      adj = adj,
      directed = FALSE,
      selfloops = FALSE
    ))

##### friendship

idfr <-
  unlist(Vectorize(FUN = maporder, vectorize.args = 'id')(id = rownames(adj), rownames(fr)))
idadj <-
  unlist(Vectorize(FUN = maporder, vectorize.args = 'id')(id = names(idfr), rownames(adj)))

fr_pred <- matrix(1, nrow(adj), ncol(adj))

fr_pred[idadj, idadj] <- fr[idfr, idfr]
fr_pred[idadj, idadj][fr_pred[idadj, idadj] == 2] <- 10
fr_pred[idadj, idadj][fr_pred[idadj, idadj] < 2] <- 1

fr_prednr <- matrix(1, nrow(adj), ncol(adj))

fr_prednr[idadj, idadj] <- fr[idfr, idfr]
fr_prednr[idadj, idadj][fr_prednr[idadj, idadj] > 0 &
                          fr_prednr[idadj, idadj] < 2] <- 10
fr_prednr[idadj, idadj][fr_prednr[idadj, idadj] == 0] <- 1

frlist <- list(fr = fr_pred, fr_nr = fr_prednr)

(fr.m <- nrm(
  w = frlist,
  adj = adj,
  directed = F,
  selfloops = F
))
AIC(fr.m)

##### facebook

idfb <-
  unlist(Vectorize(FUN = maporder, vectorize.args = 'id')(id = rownames(adj), rownames(fb)))
idadj <-
  unlist(Vectorize(FUN = maporder, vectorize.args = 'id')(id = names(idfb), rownames(adj)))

fb_pred <- matrix(1, nrow(adj), ncol(adj))

fb_pred[idadj, idadj] <- fb[idfb, idfb]
fb_pred[idadj, idadj][fb_pred[idadj, idadj] > 0] <- 10
fb_pred[idadj, idadj][fb_pred[idadj, idadj] == 0] <- 1

fb_eps <- matrix(10, nrow(adj), ncol(adj))
fb_eps[idadj, idadj] <- 1

fblist <- list(fb = fb_pred, fb_eps = fb_eps)

(fb.m <-
    nrm(
      w = fblist,
      adj = adj,
      directed = F,
      selfloops = F,
      init = c(0.1, -0.1)
    ))

###### fr + fb

(frfb.m <-
    nrm(
      w = c(frlist, fblist),
      adj = adj,
      directed = F,
      selfloops = F,
      init = c(coef(fr.m), coef(fb.m))
    ))

###### full



(full.m <-
    nrm(
      w = c(class, topic, gender, frlist, fblist),
      adj = adj,
      directed = FALSE,
      selfloops = FALSE,
      init = c(coef(tc.m), coef(homo.m), coef(frfb.m))
    ))



##### selection
# sharedp <- sharedPartner_stat(adj, directed = FALSE)

preds <-
  c(class, topic, gender, frlist, list(fblist)) #, sharedp=list(sharedp+1))
preds <- createPredictors(preds)
preds[6] <- preds[[6]]
names(preds)[6] <- 'fb'
summary(selmodels <-
          nrmSelection(
            adj,
            predictors = preds,
            directed = FALSE,
            selfloops = FALSE
          ))

gof.test(model = selmodels$models[[4]], parallel = 7)
