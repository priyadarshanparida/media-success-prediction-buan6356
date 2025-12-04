library(rpart)
library(rpart.plot)

ms <- read.csv("media-success-final-features.csv", header=TRUE, stringsAsFactors=TRUE)
ms$HIT <- factor(ms$HIT, levels = c(0,1))

set.seed(345)
train = sample(1:nrow(ms), nrow(ms)*0.7)
ms.train = ms[train,]
ms.test = ms[-train,]

fit <- rpart(
  formula = HIT ~ ., 
  data = ms.train,
  method = "class",
  control = rpart.control(
    maxdepth = 6,
    minsplit = 20,
    minbucket = 10,
    cp = 0.001
  ),
  parms = list(
    loss = matrix(c(0, 2, 1, 0), nrow = 2),
    prior = c(0.5, 0.5),
    split="gini"
  )
)

rpart.plot(fit, type = 1, extra = 1)


# ---------------------------
library(rpart)
library(rpart.plot)
library(caret)


# Load data exactly like Python
ms <- read.csv("media-success-final-features.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert boolean dummies to numeric 0/1
ms$GENRE_Hip.Hop.Rap <- as.numeric(ms$GENRE_Hip.Hop.Rap == TRUE)
ms$GENRE_Country <- as.numeric(ms$GENRE_Country == TRUE)

# Keep HIT as factor target
ms$HIT <- factor(ms$HIT, levels = c(0,1))

# Stratified train/test split equivalent to Python
set.seed(42)
library(caret)
train_idx <- createDataPartition(ms$HIT, p = 0.7, list = FALSE)

ms.train <- ms[train_idx, ]
ms.test  <- ms[-train_idx, ]

# Decision tree closest to scikit-learn parameters
fit <- rpart(
  formula = HIT ~ .,
  data = ms.train,
  method = "class",
  control = rpart.control(
    maxdepth = 6,
    minsplit = 20,
    minbucket = 10,
    cp = 0.001
  ),
  parms = list(
    prior = c(1/3, 2/3),
    split = "gini"
  )
)

# Plot
rpart.plot(fit, type = 1, extra = 1)