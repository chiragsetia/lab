df <- data.frame(matrix(runif(20000,min=1,max=200), nrow=1000))
df

df[ df>=20&df<=50 ] <- NA
df
sum(is.na(df))   #number of 
df = ifelse(is.na(df),ave(da, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$age)

