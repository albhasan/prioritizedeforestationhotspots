#' Run a experiment several times and compute its accuracy.
#'
#' This functions fits a Random Forest model several times then estimates the
#' accuracy of the iterations of the model.
#'
#' @param out_dir    A path to directory for storing results.
#' @param iterations The number of times the experiment must be repeated.
#' @return           A tibble
#'
#' @export
estimate_accuracy <- function(out_dir, iterations = 100) {
    stopifnot(dir.exists(out_dir))

    # Avoid notes during check.
    value <- file_path <- data <- metric <- .estimate <- .metric <- NULL

    data_tb <- .read_data(raw = FALSE)

    # Run the experiment XXX times and save the results.
    for (i in seq_len(iterations)) {
        print(sprintf("Processing iteration... %s", i))
        performance_file <- file.path(out_dir,
                                      paste0("performance_test_", i, ".rds"))

        # Continue from last experiment in case of an interruption.
        if (file.exists(performance_file))
            next

        #---- Data split ----
        data_split <- rsample::initial_split(data_tb)
        data_train <- rsample::training(data_split)
        data_folds <- rsample::bootstraps(data_train)

        #---- Recipe ----
        ranger_recipe <- .get_recipe(data_tb,
                                     my_formula = .build_formula())

        #---- Model ----
        ranger_spec <- .get_model()

        #---- Workflow ----
        ranger_workflow <- .get_workflow(recipe = ranger_recipe,
                                         spec = ranger_spec)

        #---- Tunning ----
        doParallel::registerDoParallel()
        ranger_tune <- .tune_model(workflow = ranger_workflow,
                                   folds = data_folds)

        #---- Finalize ----
        param_final <- ranger_tune %>%
            tune::select_best(metric = "rmse")
        saveRDS(param_final,
                file = file.path(out_dir, paste0("param_final_", i, ".rds")))
        ranger_workflow <- ranger_workflow %>%
            tune::finalize_workflow(param_final)

        #---- Evaluate ----
        data_fit <- ranger_workflow %>%
            tune::last_fit(data_split)

        test_performance <- data_fit %>%
            tune::collect_metrics()
        test_performance %>%
            dplyr::mutate(experiment = i) %>%
            saveRDS(file = performance_file)
    }

    # Evaluate the results
    crossvalidation_tb <- out_dir %>%
        list.files(pattern = "performance_test*.",
                   full.names = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(data = purrr::map(file_path, readRDS)) %>%
        dplyr::pull(data) %>%
        dplyr::bind_rows()
    crossvalidation_tb %>%
        readr::write_csv(file = file.path(out_dir, "crossvalidation_tb.csv"))

    crossvalidation_tb %>%
        dplyr::group_by(.metric) %>%
        dplyr::summarize(mean = mean(.estimate)) %>%
        (function(x) {
            print(x, n = Inf)
            return(x)
        }) %>%
        return()
}



#' Fits a model to the data in the package.
#'
#' @param out_dir   A path to directory for storing results.
#' @return          A tibble
#'
#' @export
fit_model <- function(out_dir) {
    .pred <- NULL
    stopifnot(dir.exists(out_dir))

    data_tb <- .read_data(raw = FALSE)

    #---- Data split ----
    data_train <- data_tb
    set.seed(42)
    data_folds <- rsample::bootstraps(data_train)

    #---- Recipe ----
    ranger_recipe <- .get_recipe(data_tb,
                                 my_formula = .build_formula())

    #---- Model ----
    ranger_spec <- .get_model()

    #---- Workflow ----
    ranger_workflow <- .get_workflow(recipe = ranger_recipe,
                                     spec = ranger_spec)

    #---- Tunning ----
    doParallel::registerDoParallel()
    ranger_tune <- .tune_model(workflow = ranger_workflow,
                               folds = data_folds)

    #---- Finalize ----
    param_final <- ranger_tune %>%
        tune::select_best(metric = "rmse")

    ranger_workflow <- ranger_workflow %>%
        tune::finalize_workflow(param_final)
    print("Final model's hyper-parameters...")
    print(ranger_workflow)

    #---- Evaluate ----
    # NOTE: We're using ALL the samples for training, so, we don't need this.

    #---- Importance of variables ----
    imp_spec <- ranger_spec %>%
        tune::finalize_model(tune::select_best(ranger_tune,
                                         metric = "rmse")) %>%
        parsnip::set_engine("ranger",
                            importance = "permutation")

    workflow_fit <- workflows::workflow() %>%
        workflows::add_recipe(ranger_recipe) %>%
        workflows::add_model(imp_spec) %>%
        parsnip::fit(data_train) %>%
        workflows::extract_fit_parsnip()
    workflow_fit %>%
        magrittr::extract2("fit") %>%
        magrittr::extract2("variable.importance") %>%
        as.list() %>%
        tibble::as_tibble() %>%
        readr::write_csv(file = file.path(out_dir, "variable_importance.csv"))

    workflow_fit %>%
        vip::vip(aesthetics = list(alpha = 0.8,
                              fill = "midnightblue"))
    ggplot2::ggsave(file.path(out_dir, "feature_importance.png"))

    #---- Fitting and using ----
    final_model <- parsnip::fit(ranger_workflow,
                                data_tb)
    saveRDS(final_model,
            file = file.path(out_dir, "final_model.rds"))

    # Save the prediction on the original data.
    raw_tb <- .read_data(raw = TRUE)
    fake_new_data <- ranger_recipe %>%
        recipes::prep(raw_tb) %>%
        recipes::juice()
    fake_pred <- stats::predict(final_model, fake_new_data)
    new_data_tb <- raw_tb %>%
        dplyr::bind_cols(fake_pred) %>%
        dplyr::mutate(pred_def = exp(.pred)) %>%
        (function(x) {
            saveRDS(x, file = file.path(out_dir, "new_data_tb.rds"))
            readr::write_csv(x, file = file.path(out_dir, "new_data_tb.csv"))
            return(x)
        })
}



#' Read and prepare the data for the experiment.
#'
#' This functions read the deforestation data from the package
#' (`deforestation_data`), then it transforms their areas from km2 to mt2, it
#' applies a `log` transformation, and finally it filters out the data with no
#' deforestation.
#'
#' @param raw A logical. When TRUE, the whole data set is returned. Otherwise,
#'            only those registers where deforestation is greater than zero are
#'            returned.
#'
#' @return A tibble.
.read_data <- function(raw = FALSE) {
    # Avoid notes during check.
    def <- NULL

    # Prepare data.
    data_tb <- prioritizedeforestationhotspots::deforestation_data %>%
        dplyr::mutate(def = def * 1000^2,
                      log_def = log(def))

    # Filter.
    if (!raw) {
        data_tb <- data_tb %>%
            dplyr::filter(def > 0)
    }

    return(data_tb )
}



#' Build models for the given year.
#'
#' This function builds formula objects.
#'
#' @return        A formula object.
.build_formula <- function() {
    form <- NA
    form <- stats::as.formula(
        "log_def ~ def_1_ly + def_2_ly + def_4_ly +
                   dist_1_percent_ly + dist_2_percent_ly +
                   dist_hidro + dist_road + dist_road_hidro +
                   area_PA + active_fires_ly"
    )
    return(form)
}



#' Set the recipe to be used.
#'
#' This function sets the formula to be used by the classification model.
#'
#' @param data_tb    A tibble.
#' @param my_formula A formula object.
#' @return           A recipe object.
.get_recipe <- function(data_tb, my_formula) {
    ranger_recipe <- recipes::recipe(
        formula = my_formula,
        data = data_tb
    ) %>%
        return()
}



#' Set the model.
#'
#' This function sets the parameters of the model used.
#'
#' @param trees An integer. The number of trees.
#' @return A random forest model.
.get_model <- function(trees = 2000) {
    parsnip::rand_forest(mtry = tune::tune(),
                         min_n = tune::tune(),
                         trees = trees) %>%
        parsnip::set_mode(mode = "regression") %>%
        parsnip::set_engine("ranger") %>%
        return()
}



#' Set the workflow.
#'
#' This functions set the workflow of the model.
#'
#' @param recipe  A recipe object.
#' @param spec    A model specification.
#' @return        A workflow object.
.get_workflow <- function(recipe, spec) {
    workflows::workflow() %>%
        workflows::add_recipe(recipe) %>%
        workflows::add_model(spec) %>%
        return()
}



#' Tune model
#'
#' This functions tunes the model parameters
#'
#' @param workflow A workflow object.
#' @param folds    A fold object.
#' @param grid     An integer.
#' @return         A tune_grid object.
.tune_model <- function(workflow, folds, grid = 11) {
    tune::tune_grid(workflow,
                    resamples = folds,
                    grid = grid) %>%
        return()
}

