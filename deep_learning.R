# Housekeeping ------------------------------------------------------------
library(keras)
library(magrittr)
library(dplyr)
library(tidyr)

# Data  -------------------------------------------------------------------
load("Stanage.Rds")
crag_climb_data_flat <- read.csv("crag_climb_data_flat.csv")
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("crag_"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("id_buttress_"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("discipline_"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("grade_climb_"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("style_climb_"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("n_logs"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("n_partners"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("hidden"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("day_climb"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("month_climb"))
#crag_climb_data_flat <- crag_climb_data_flat %>% select(-starts_with("year_climb"))

# Remove Trailing Underscore
crag_climb_data_flat <- crag_climb_data_flat %>% setNames((tolower(gsub("_$","",names(.)))))
crag_climb_data_flat <- crag_climb_data_flat %>% setNames((tolower(gsub("_$","",names(.)))))

#Remove Duplicate Column Names (Not Sure Why This Happens)
crag_climb_data_flat <- crag_climb_data_flat %>% setNames(make.names(names(.), unique = TRUE))

# NA occurs if no comments.
# Setting NA to "-2
# Transforming categories from -2:3 to 0:5 to work with keras::to_categorical function.
crag_climb_data_flat <- crag_climb_data_flat %>% mutate(rating_climb = ifelse(is.na(rating_climb), -2, rating_climb),
                                                        rating_climb = rating_climb + abs(min(rating_climb)))
# Dropping Climbs with Missing Data
crag_climb_data_flat <- crag_climb_data_flat %>% tidyr::drop_na()

# Removing non A-Z, 0-9 charachters from column names
crag_climb_data_flat <- crag_climb_data_flat %>% setNames((tolower(gsub("[^A-Za-z0-9_]","xxx",names(.)))))

#Remove Duplicate Column Names (Not Sure Why This Happens)
crag_climb_data_flat <- crag_climb_data_flat %>% setNames(make.names(names(.), unique = TRUE))

# Split into Train/Test
set.seed(1337)
smp_size <- crag_climb_data_flat %>% nrow() %>% multiply_by(0.75) %>% floor()
train_ind <- crag_climb_data_flat %>% nrow() %>% seq_len() %>% sample(size = smp_size)
train <- crag_climb_data_flat[train_ind, ]
test <- crag_climb_data_flat[-train_ind, ]


# Checking Data
train_data <- train %>% select(-rating_climb)
train_labels <- train %>% use_series(rating_climb) %>% to_categorical()

test_data <- test %>% select(-rating_climb)
test_labels <- test %>% use_series(rating_climb) %>% to_categorical()

# Data transformation ----------------------------------------------------
x_train <- train_data %>% as.matrix()
x_test <- test_data %>% as.matrix()

y_train <- train_labels
y_test <- test_labels
#y_train <- as.numeric(train_labels)
#y_test <- as.numeric(test_labels) 

# Model -------------------------------------------------------------------
network <- keras_model_sequential() %>%
  # 16 neurons in first layer, input shape of data.
  # One hidden layer is sufficient for the majority of problems.
  # https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw
  layer_dense(units = 1034, activation = "tanh", input_shape = c(1169)) %>%
  # second layer of 16 neurtons
  #layer_dense(units = 100, activation = "tanh") %>%
  # one output layer
  layer_dense(units = 6, activation = "softmax")

# Why not using Stochastic Gradient descent and defining a learning rate?
sgd <- optimizer_sgd(lr = 0.01)
network %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# Run 
network %>% fit(x_train, y_train, epochs = 128, batch_size = 256)

# predict the likelihood that a review is positive
network %>% predict(x_test[1:10,])
network %>% predict_classes(x_test)


temp <- test
temp$prediction <- network %>% predict_classes(x_test)
temp$wrong <- abs(temp$prediction - temp$rating_climb)
temp[c("id_climb","rating_climb","prediction", "wrong")] %>% arrange(desc(wrong))
table(temp$prediction)
table(temp$wrong)