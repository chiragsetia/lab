df <- data.frame(matrix(runif(20000,min=50,max=100), nrow=2000))
df
boxplot(df, use.cols = TRUE)
hist(df)
plot(df$X3,df$X5)