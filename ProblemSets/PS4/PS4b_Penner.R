data("iris")
df <- iris

df <- createDataFrame(df)

class(df)
class(df1)

head(select(df, df$Sepal_length, df$Species))
head(select(df1, df1$Sepal_length, df1$Species))

head(filter(df, df$Sepal_length>5.5))
head(filter(df1, df1$Sepal_length>5.5))

head(select(filter(df, df$Sepal_length>5.5), df$Sepal_length, df$Species))

head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_length), count=n(df$Sepal_length)))

df2 <- head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_length), count=n(df$Sepal_length)))
head(arrange(df2, asc(df2$Species)))

