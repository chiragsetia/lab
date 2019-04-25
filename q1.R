df <- data.frame(matrix(c(runif(300,min=1,max=10),runif(400,min=0,max=1),runif(300,min=100,max=200)), nrow=100))
df

library("cluster")
library("factoextra")
library("magrittr")

res.dist <- get_dist(df, stand = TRUE, method = "pearson")
fviz_dist(res.dist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(my_data, kmeans, method = "gap_stat")  
set.seed(123)

#K-means Clustering
km.res <- kmeans(df, 3, nstart = 25)
fviz_cluster(km.res, data = df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Hierarchical clustering
res.hc <- df %>%
  scale() %>%                    
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     


fviz_dend(res.hc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE 
)