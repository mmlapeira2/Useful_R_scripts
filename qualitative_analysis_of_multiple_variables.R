# libraries to load
library(ggplot2)
library(rcompanion)
library(multcompView)
# check working directory
getwd()

## Dummy DF ##

df <- data.frame("Class" = factor(sample(1:3, 100, replace = T), 
                                  labels = c("Co", "Treat_1", "Treat_2")),
                 "Var1" = sample(1:5, 100, replace = T),
                 "Var2" = sample(1:10, 100, replace = T))
df[df$Class == "Treat_1",2] <- df[df$Class == "Treat_1",2] *1.75
df[df$Class == "Treat_2",3] <- df[df$Class == "Treat_2",3] *1.5
# introduim algun outlier
df[sample(1:100, 1),2:3] <- c(10,20)
df[sample(1:100, 1),2:3] <- c(-3,0)

## Load your DF if you have it ##

# df <- read.delim("./path/data.txt", 
#                  stringsAsFactors = T) # set to T if the character variables are factors

## Visualize DF ##
str(df)

summary(df)

#### HISTOGRAM, BOXPLOT AND OUTLIERS ####

hist(df$Var1) # basic histogram function
hist(df$Var1, 
     main = "Histogram of Variable 1", # change the title
     xlab = "Var1 (units)" # change the x labeling
)

ggplot(df, aes(x=Var1, fill=Class)) + geom_histogram() # ggplot option

## how to create a loop ##
for (i in 2:ncol(df)) {
  print(i)
}

for (i in 2:ncol(df)) { #histogram loop
  # create the plot and store it #
  plt <- ggplot(df, aes(x=df[,i], # access the ith variable of df
                        fill =Class, color=Class)) +
    geom_histogram(alpha=0.5, position="identity") + # options to increase
                                                    # the transparency of plot
    xlab(colnames(df)[i]) # display variable name on x axis
  
  # open png handle, print plot and close handle
  png(file= paste("hist_", colnames(df)[i],".png", sep = ""))
  print(plt)
  dev.off()
}

for (i in 2:ncol(df)) { # boxplot
  plt <- ggplot(df, aes(y=df[,i], x =Class, color=Class)) +
    geom_boxplot() +
    ylab(colnames(df)[i])
  png(file=paste("boxplot_", colnames(df)[i],".png", sep = ""))
  print(plt)
  dev.off()
}

## OUTLIER SEARCH ## 

outliers <- list() # open a list to store the values

for (i in 2:ncol(df)) { # loop through variables
  
  # values lower than the mean minus the confidence interval or
  # grater than the mean plus the confidence interval
  out <- which(df[,i] < mean(df[,i]) - qt(0.99,nrow(df)-1)*sd(df[,i]) |
               df[,i] > mean(df[,i]) + qt(0.99,nrow(df)-1)*sd(df[,i]))
  
  # store values on list
  outliers <- append(outliers, list(out))
  
}

names(outliers) <- colnames(df)[-1] # name the list with the outlier values
print(outliers)

# check the values of the outliers and erase them
df[outliers$Var1,]
df[outliers$Var1,] <- NA
df[outliers$Var2,]
df[outliers$Var2,] <- NA

# Now we can re-do the histograms and boxplot

#### PAIRWISE COMPARISON ####

# create an empty table to store the results
tblres <- data.frame(matrix(nrow = ncol(df)-1, 
                            ncol = length(levels(df$Class))*3))
rownames(tblres) <- colnames(df)[-1]
colnames(tblres) <- paste(rep(c("mean", "sd", "Signif"), length(levels(df$Class))), 
                          rep(levels(df$Class), each=length(levels(df$Class))))


for (i in 2:ncol(df)) {
  
  # store mean and SD by group
  tblres[i-1,seq(1,ncol(tblres), by=3)] <- aggregate(df[,i], list(df$Class), FUN=mean)$x
  tblres[i-1,seq(2,ncol(tblres), by=3)] <- aggregate(df[,i], list(df$Class), FUN=sd)$x
  
  # perform t.test between groups and store letters
  PT <- pairwise.t.test(df[,i], df$Class, p.adj="bonf")$p.value
  PT1 = fullPTable(PT)
  tblres[i-1,seq(3,ncol(tblres), by=3)] <- 
    multcompLetters(PT1, 
                    compare="<", threshold=0.05, 
                    Letters=letters)$Letters
}


