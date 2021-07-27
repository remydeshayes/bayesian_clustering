library(mclust)

##############################
### Model-Based Clustering ###
###### Galaxy Dataset ########
##############################

#Setting seed for the subsampling 
set.seed(4)

#Loading galaxy dataset from MASS package
data(galaxies, package = "MASS")
galaxiespp <- galaxies / 1000

# Initialization by sub-sampling  

S <- sample(1: length(galaxiespp), size=25)
galaxies_sample <- densityMclust(galaxiespp, modelNames = "V", initialization = list(subset=S))
galaxies_sample$BIC
plot(galaxies_sample, what = 'BIC', legendArgs = list(x = "bottomleft"))
summary(galaxies_sample, parameters=TRUE)

galaxies_sample$classification

# Initialization hierarchical model based clustering

galaxies_hierarchical <- densityMclust(galaxiespp, modelNames = "V", initialization = list(hcPairs = hc(galaxies)))
galaxies_hierarchical$BIC
plot(galaxies_hierarchical, what = 'BIC', legendArgs = list(x = "bottomleft"))
summary(galaxies_hierarchical, parameters=TRUE)
galaxies_hierarchical$classification

plot(galaxies_sample, what = "density", data = galaxiespp, breaks = 20, add= TRUE)
plot(galaxies_hierarchical, what = "density", data = galaxiespp, breaks = 20, add= TRUE)

#####################################
### Clustering Method Comparisons ###
######### Wine Dataset    ########### 
#####################################

library(pgmm)
data(wine)
# Separating Class and features
wine.data <- wine[2:28]
wine.class <- unlist(wine[1])

# Model-Based-Clustering model selection, mixture components selection and classification error computation
start_time_mbc = Sys.time()
wine.Mclust <- Mclust(wine.data)
end_time_mbc = Sys.time()
mbc_time = end_time_mbc - start_time_mbc #compute computational time
  
summary(wine.Mclust)
plot(wine.Mclust , what = "classification")
classError(wine.Mclust$classification, wine.class)
plot(wine.Mclust$BIC)

# K-means clustering and classification error computation
start_time_kmeans = Sys.time() 
wine.kmeans <- kmeans (wine.data , centers=3, nstart =20)
end_time_kmeans = Sys.time()
kmeans_time = end_time_kmeans - start_time_kmeans #compute computational time
kmeans_time 

classError(wine.kmeans$cluster,wine.class)

# Hierarchical clustering and classification error computation
start_time_h = Sys.time()
wine.hclust <- hclust(dist(wine.data),method="complete")
end_time_h = Sys.time()
h_time = end_time_h - start_time_h #compute computational time
h_time
wine.complete <- rect.hclust(wine.hclust, k=3)
n.wine <- length(wine.data[,1])
wine.complete.class <- rep (NA , n.wine)
for (g in 1:3){ wine.complete.class [wine.complete.class [[g]]] <- g}
classError (wine.complete.class , wine.class)



