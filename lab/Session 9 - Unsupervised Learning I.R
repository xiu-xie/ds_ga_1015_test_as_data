# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 03/31/2022
# Lab adapted from: Leslie Huang, Pedro L. Rodriguez and Lucia Motolinia

# Set up workspace
rm(list = ls())

# Loading packages
#install.packages("lsa")
#install.packages("factoextra")
#install.packages("text2vec")

library(quanteda)
library(quanteda.corpora)
library(dplyr)
# makes it easy to work with PCA (great for visualization)
library(factoextra) 

library(text2vec)
library(lsa)

#set directory
setwd("F:/R/textasdata2022/W9_03_31_22")


## 1 PCA 


# 1.1 Two functions in base R for PCA:
# see: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#?prcomp # uses the singular value decomposition approach: examines the covariances/correlations between individuals
#?princomp # uses the spectral decomposition (eigendecomposition) approach: examines the covariances/correlations between variables (need more individuals than variables)

# Remember to center your data! (default = TRUE) -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!


# 1.2 Example
data("data_corpus_sotu")
SOTU <- corpus_subset(data_corpus_sotu, Date > "1900-01-01")

SOTU_dfm <- dfm(SOTU, 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)

SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

View(SOTU_mat)

# run pca
SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

SOTU_pca

summary(SOTU_pca)
dim(SOTU_pca$x)
View(SOTU_pca$x[1:100,1:3])

# visualize eigenvalues (scree plot: shows percentage of variance explained by each dimension)
fviz_eig(SOTU_pca, addlabels = TRUE)
#how many relevant dimensions are there?

# PCA is essentially a rotation of the coordinate axes, 
# chosen such that each successful axis 
# captures as much variance as possible

# Loadings for each variable: columns contain the eigenvectors
SOTU_pca$rotation[1:10, 1:5]
dim(SOTU_pca$rotation)

# Can we interpret the dimensions?
pc_loadings <- SOTU_pca$rotation

View(pc_loadings)

# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  # these should be orthogonal
# moderately correlated

# More on prcomp : https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/#:~:text=The%20prcomp%20function%20takes%20in,variance%20one%20before%20doing%20PCA.&text=We%20have%20stored%20the%20results,variables%20associated%20with%20the%20analysis.

# top loadings on PC1
# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
View(pc1_loading)

pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)

#lets get the top and bottom 10
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))

View(pc1_loading)

# plot top tokens according to absolute loading values
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank())

# Value of the rotated data: your "new", dimensionality reduced data
View(SOTU_pca$x)  # each observation 

# retrieve most similar documents

# function computes cosine similarity 
# between query and all documents and returns N most similar

nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}

# apply to document retrieval
nearest_neighbors(query = "Obama-2009", low_dim_space = SOTU_pca$x, N = 5, norm = "l2")

# Visualization resources:

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

## 2 Latent Semantic Analysis (LSA) aka Latent Semantic Indexing (LSI) 

# foundational theory: distributional hypothesis
# what is the context in LSA? document

# Let's keep using the SOTU data from before
SOTU_mat_lsa <- convert(SOTU_dfm, to = "lsa") # convert to transposed matrix 

SOTU_mat_lsa

#(so terms are rows and columns are documents = TDM)
SOTU_mat_lsa <- lw_logtf(SOTU_mat_lsa) * gw_idf(SOTU_mat_lsa) 
# local - global weighting (akin to TFIDF)

SOTU_mat_lsa

# 2.1 Create LSA weights using TDM
#lsa(myMatrix, dims = dimcalc_share(share = 0.8))

# dimcalc : Methods for choosing a 'good' number 
# of singular values for the dimensionality reduction in LSA

# share = fraction of the sum of the selected singular 
# values over the sum of all singular values, default is 0.5

# https://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/packages/lsa.pdf

# https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.564.3872&rep=rep1&type=pdf

SOTU_lsa <- lsa(SOTU_mat_lsa)

# lsa returns a latent semantic space 
# in textmatrix format: rows are terms, columns are documents

View(SOTU_lsa)
as.textmatrix(SOTU_lsa)

# what do we expect this correlation to be?
cor(SOTU_lsa$tk[,1], SOTU_lsa$tk[,2])  # these should be orthogonal

View(SOTU_lsa)
# lsa_obj$tk = truncated term matrix from term vector matrix T (constituting left singular vectors from the SVD of the original matrix)
# lsa_obj$dk = truncated document matrix from document vector matrix D (constituting right singular vectors from the SVD of the original matrix)
# lsa_obj$sk = singular values: Matrix of scaling values to ensure that multiplying these matrices reconstructs TDM
# see: https://cran.r-project.org/web/packages/lsa/lsa.pdf

# Setting dim = 5
SOTU_lsa_5 <- lsa(SOTU_mat_lsa, 5)

# display generated LSA space
#?as.textmatrix
SOTU_lsa_5_mat <- t(as.textmatrix(SOTU_lsa_5))

SOTU_lsa_5_mat

# Q: What are these documents about?
# Compare features for a few speeches
SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

# With 5 dims:
sort(SOTU_lsa_5_mat[9,], decreasing=T)[1:10]

# With auto (43) dims:
sort(t(as.textmatrix(SOTU_lsa))[9, ], decreasing = T)[1:10]

# Q: How are words related?
# associate(): a method to identify words that are most 
# similar to other words using a LSA
#?associate
# uses cosine similarity between input term and other terms
SOTU_lsa_mat <- as.textmatrix(SOTU_lsa) 

oil <- associate(SOTU_lsa_mat, "oil", "cosine", threshold = .7)
oil[1:10]

health <- associate(SOTU_lsa_mat, "health", "cosine", threshold = .7)
health[1:10]

# Keep this in mind when we do topic models!