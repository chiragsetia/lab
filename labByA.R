#1)
A = matrix( 
  c(runif(3000, min=1, max=200)), # the data elements 
  nrow=100,              # number of rows 
  ncol=30,              # number of columns 
  byrow = FALSE)        # fill matrix by column 

A.df<-as.data.frame(A)

for(i in 1:100){
  for(j in 1:30){
    if(A.df[i,j]<60&A.df[i,j]>10){
      A.df[i,j]=NA;
    }
  }
}

count = 0;

for(i in 1:100){
  for(j in 1:30){
    if(is.na(A.df[i,j])){
      count = count+1;
    }
  }
}

print(count);

for(i in 1:ncol(A.df)){
  A.df[is.na(A.df[,i]), i] <- mean(A.df[,i], na.rm = TRUE);
}


pairCor <- cor(A.df,use="pairwise",method="pearson")



for(i in 1:100){
  for(j in 1:30){
    if(pairCor<=0.7){
      print(pairCor);
    }
  }
}

preprocessParams <- preProcess(A.df[,1:30], method=c("range"), rangeBounds = c(0, 10))

print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, A.df[,1:30])
# summarize the transformed dataset
summary(transformed)

for(i in 1:100){
  for(j in 1:30){
    if(transformed[i,j]<=0.5){
      transformed[i,j]=0;
    }
    else
      transformed[i,j]=1;
  }
}


#Q3
B = matrix( 
  c(runif(9000, min=-100, max=100)), # the data elements 
  nrow=600,              # number of rows 
  ncol=15,              # number of columns 
  byrow = FALSE)        # fill matrix by column 

B.df<-as.data.frame(B)

plot(B.df$V5, B.df$V6)

multi.hist(B.df)

boxplot(B.df)

#Q4
C = matrix( 
  c(runif(2500, min=5, max=10)), # the data elements 
  nrow=500,              # number of rows 
  ncol=5,              # number of columns 
  byrow = FALSE)        # fill matrix by column 

C.df<-as.data.frame(C)

for(i in 1:5){
  print(t.test(C.df[,i], mu=10))
  print(wilcox.test(C.df[,i], mu=20, conf.int = TRUE))
}

print(wilcox.test(C.df$V3, B.df$V4, alternative = "g"))