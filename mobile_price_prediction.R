used_packages <- c("Amelia", "caret", "dplyr", "e1071", "futile.logger", 
                   "GGally", 'ggplot2', "ISLR", "LaplacesDemon", "readr", 
                   "reticulate", "viridis")
lapply(used_packages, require, character.only = TRUE)

flog.info("__author__ : Nezih YALABIK")
# It was taken in consideration Google R Style Guide 
# for codes to be read easily, shared, and verified!
# For example; Maximum Line Length = 80


dataset <- read_csv("Desktop/nezih_yalabık_project/train.csv")
View(dataset)

# row count and length of data
nrow(dataset)
length(dataset)

# dimension of data (row count + length)
dim(dataset)

# general info about data
summary(dataset)

# the first six and last six row of data
head(dataset) 
tail(dataset)

# columns names
colnames(dataset)

# missing data visualization
flog.info("If data are big, it takes the time to appear the plot/graphic/image, 
          please wait a little!")
missmap(dataset)


# Correlation plot
ggcorr(dataset, palette = "RdBu", label = TRUE)
ggpairs(dataset)


# How does ram is affected by price
ggplot(dataset, aes(x = dataset$ram, y = dataset$price_range)) +
  stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile')

ggplot(dataset, aes(x = dataset$ram, y = dataset$price_range)) +
  stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile') +
  scale_fill_viridis()

# Internal Memory vs Price Range
ggplot(dataset, aes(x = dataset$price_range, y = dataset$int_memory)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") + theme_minimal()


# % of Phones which support 3G
  # plot 1
values <- table(dataset$three_g)
lbls <- paste(c("Not supported", "3G-supported"), "\n", values, sep="")
pie(values, labels = lbls)

  # plot 2
threeG_supporting_status <- dataset[, c("three_g")]
threeG_supporting_status$three_g_group <- ifelse(
  threeG_supporting_status$three_g == 0,
  'Not supported',
  '3G-supported'
  )

threeG_supporting_status_grp <- 
  threeG_supporting_status %>%
  group_by(three_g, three_g_group) %>%
  summarise(count = n(), percent=(n()/nrow(threeG_supporting_status))*100)

ggplot(threeG_supporting_status_grp, aes(x="", y=percent, fill=three_g_group)) +
  geom_bar(width = 1, stat = "identity") 

ggplot(threeG_supporting_status_grp, aes(x="", y=percent, fill=three_g_group)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)


# Battery power vs Price Range

boxplot(dataset$price_range, dataset$battery_power, data=dataset,
        xlab = 'price_range', ylab = 'battery_power',
        notch = TRUE, col = c("orange", "blue"))

##Explore the data.
ggplot(dataset, aes(dataset$price_range)) + geom_density(fill="blue")
ggplot(dataset, aes(log(dataset$price_range))) + geom_density(fill="blue")
ggplot(dataset, aes(sqrt(dataset$price_range))) + geom_density(fill="blue")


# To split the dataset into training and test

train_index <- sample(1:nrow(dataset), 0.77 * nrow(dataset))
test_index <- setdiff(1:nrow(dataset), train_index)

X_train <- dataset[train_index, -21]
y_train <- dataset[train_index, "price_range"]

X_test <- dataset[test_index, -21]
y_test <- dataset[test_index, "price_range"]

# Fit a linear regression line
fit = lm(dataset$price_range ~ ., data = dataset)
# Display details of fir
summary(fit)
# Display the confidence intervals
confint(fit)



# KNN
# Transforming the dependent variable to a factor
dataset$price_range = as.factor(dataset$price_range)

#Partitioning the data into training and validation data
set.seed(101)
index = createDataPartition(dataset$price_range, p = 0.77, list = F )
train = dataset[index,]
validation = dataset[-index,]

# Explore data
dim(train)
dim(validation)
names(train)

# Setting levels for both training and validation data
levels(train$price_range) <- make.names(levels(factor(train$price_range)))
levels(validation$price_range) <- make.names(levels(factor(validation$price_range)))

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE)

model <- caret::train(price_range ~ .,
                      data = train, 
                      method = 'knn',  
                      preProcess = c('center', 'scale'), 
                      trControl = x, 
                      metric = 'ROC', 
                      tuneLength = tunel)

# Summary of model
model
plot(model)
