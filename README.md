# EuroCSS 2019: Introduction to Multi-edge Network Inference in R Using the Ghypernet-package

Repository for the EuroCSS Tutorial "Introduction to Multi-edge Network Inference in R Using the Ghypernet-package"

## Set up/Installation

Please download the repository, or better clone it to your desktop. You can 
also create an R-project (that automatically clones the repository for you and
creates a project for you).

Please load the package `r-hypernets`: 

```{r}
library(devtools)
install_github("sg-dev/r-ghypernet")
```

## Program

- 9:00: start Tutorial, introduction round
- 9:15-10:30: Introduction to generalized hypergeometric ensembles
- 10:30-11:00: Coffee break
- 11:00-11:30: Endogenous network patterns and comparisons to exponential random graph models (ERGMs)
- 11:30-12:30: R tutorial 

## Check out some additional literature

The folder `Literature` contains some pre-prints/articles on generalized
hypergeometric ensembles and gHypEG regressions.

- Giona Casiraghi, Vahan Nanumyan, Ingo Scholtes, and Frank Schweitzer. Generalized Hypergeometric Ensembles: Statistical Hypothesis Testing in Complex Networks. arXiv preprint arXiv:1607.02441, jul 2016.
- Giona Casiraghi. Multiplex Network Regression: How do relations drive interactions? arXiv preprint arXiv:1702.02048, feb 2017.
- Giona Casiraghi and Vahan Nanumyan. Generalised hypergeometric ensembles of random graphs: the configuration model as an urn problem. arXiv preprint arXiv:1810.06495, oct 2018.
- Laurence Brandenberger, Giona Casiraghi, Vahan Nanumyan, & Frank Schweitzer. Quantifying Triadic Closure in Multi-Edge Social Networks. arXiv preprint arXiv:1905.02990, 2019.

Literature on inferential network models: 

- Skyler J Cranmer, Philip Leifeld, Scott D McClurg and Meredith Rolfe. 2017. “Navigating the range of statistical tools for inferential network analysis.” American Journal of Political Science 61(1):237–251.
- David R Hunter, Steven M Goodreau, and Mark S Handcock. Goodness of fit of social network models. Journal of the American Statistical Association, 103(481): 248–258, 2008.
- Tom AB Snijders, Philippa E Pattison, Garry L Robins, and Mark S Handcock. New specifications for exponential random graph models. Sociological methodol- ogy, 36(1):99–153, 2006.
- Martina Morris, Mark S Handcock, and David R Hunter. Specification of exponential-family random graph models: terms and computational aspects. Journal of statistical software, 24(4):1548, 2008.
- Bruce A Desmarais and Skyler J Cranmer. Micro-level interpretation of exponen- tial random graph models with application to estuary networks. Policy Studies Journal, 40(3):402–434, 2012.

## Cite the package

You can cite the package in your articles/reports as follows:

> Giona Casiraghi and Vahan Nanumyan. ghypernet: R package for Generalised Hypergeometric Ensembles of Random Graphs (gHypEG), 2019. R package version 0.5.0. https://github.com/gi0na/r-ghypernet/.
