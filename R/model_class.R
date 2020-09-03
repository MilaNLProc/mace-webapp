#' R6 Class Representing a Model Object
#'
#' @description
#' A model object containing the model data and fitted models including
#' linear regression, decision tree and random forest models.
#'
Model <-
  R6::R6Class("Model", public = list(
          #' @field dataframe of features and target variable
          data = NA,

          #' @field character, selected algorithm
          algos = NA,

          #' @field character, target variable column name
          target_variable = NA,

          #' @description
          #' Create a new Model object, load and prepare the model data
          #' @param data_csv character, the location of the model data
          #' @param target_variable character, the name of the target column
          #' @return A new `Model` object.
          initialize = function(data_csv, target_variable) {
            stopifnot(is.character(data_csv))
            stopifnot(is.character(target_variable))

            self$data <- self$prepare_data(data_csv)
            stopifnot(target_variable %in% names(self$data))

            self$algos <- c("linear regression", "decision tree",
                            "random forest")
            self$target_variable <- target_variable
            },

          #' @description
          #' Import data, fill missing values and create features
          #' @param data_file character, file name of model data
          #' @return dataframe of model features and target variable
          prepare_data = function(data_file) {
            housing_data <- readr::read_csv(data_file)

            median_beds <- median(housing_data$total_bedrooms, na.rm = TRUE)

            # replace missing values and add features
            housing_data %>%
              mutate(total_bedrooms = coalesce(total_bedrooms, median_beds),
                     rooms_per_household = total_rooms / households,
                     bedrooms_per_room = total_bedrooms / total_rooms,
                     population_per_household = population / households)
          },

          #' @description
          #' Fit model with selected algorithm and features
          #' @param algo character, type of model to be used
          #' @param features character, names of features to include
          #' @return list of predicted values and R-Squared metric
          fit_model = function(algo, features) {
            stopifnot(algo %in% self$algos)
            stopifnot(all(features %in% names(self$data)))

            # model target variable as function of all features
            model_formula <- as.formula(paste0(self$target_variable, " ~ ."))

            if (algo == "linear regression") {
              model_func <- lm
              model_data <- select(self$data, features, self$target_variable)
            }
            else if (algo  == "decision tree") {
              model_func <- partykit::ctree

              # replace categorical variables with numeric
              model_data <- self$data %>%
                select(features, self$target_variable) %>%
                mutate_if(is.character, as.factor) %>%
                mutate_if(is.factor, as.integer)
            }
            else {
              model_func <- randomForest::randomForest

              # replace categorical variables with numeric
              model_data <- self$data %>%
                select(features, self$target_variable) %>%
                mutate_if(is.character, as.factor) %>%
                mutate_if(is.factor, as.integer)
            }

            # fit model
            model <- model_func(model_formula, model_data)

            predictions <-
              data.frame(actual = pull(self$data, self$target_variable),
                         preds = predict(model, newdata = self$data))

            list(predictions = predictions,
                 r_squared = cor(predictions$actual, predictions$preds) ^ 2)
          })
        )

