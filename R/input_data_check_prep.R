
input_data_check_prep <- function(
  train_data,
  train_targets=NULL,
  test_data=NULL,
  test_targets=NULL,
  target_columns=NULL,
  train_only=FALSE,
  split_data=FALSE
){

  if(!is.null(target_columns) & !is.null(train_targets)){
    # DF + colnames
    assertthat::assert_that(length(target_columns) > 0)
    assertthat::assert_that(all(target_columns %in% colnames(train_targets)))
    assertthat::assert_that(
      assertthat::are_equal(nrow(train_data), nrow(train_targets))
    )
    assertthat::assert_that(
      is.data.frame(train_targets) | is.matrix(train_targets)
    )
    # Make sure samples for the given target exist for training (!=0)
    purrr::walk(target_columns, function(target){

      is_int_trgt_vector <- all(train_targets[, target] %% 1 == 0)

      ## int vector with target classes
      assertthat::assert_that(is_int_trgt_vector)

      samples_in_target <- sum(as.integer(train_targets[, target]))

      assertthat::assert_that(
        samples_in_target > 0,
        msg = paste0(
          "Target class: '", target, "' has 0 samples to train for.", "\n",
          "Make sure samples for the given target exist for training."
        )
      )
      return(NULL)
    })

  }else if(is.null(target_columns) & !is.null(train_targets)){

    # TODO: if named vector ensure it has same order as rownames for train data

    # vector w/ target classes
    assertthat::assert_that(
      assertthat::are_equal(nrow(train_data), length(train_targets))
    )

    is_chr_trgt_vector <-
      is.character(train_targets) | is.factor(train_targets)

    is_int_trgt_vector <- all(train_targets %% 1 == 0)

    assertthat::assert_that(
      ## chr/factor vector with target classes
      is_chr_trgt_vector |
      ## int vector with target classes
      is_int_trgt_vector
    )

    assertthat::assert_that(unique(train_targets) > 1)

    train_targets <-
      train_targets %>%
      make.names() %>%
      as.factor() %>%
      data.table::as.data.table() %>%
      data.table::setnames("V1", "class") %>%
      mltools::one_hot(dropCols = FALSE)

    is_binary_class <-
      train_targets %>% dplyr::pull(class) %>% levels() %>% length() == 2

    if(is_binary_class){
    # if class only has 2 levels, drop the ohe column for the one which is larger
      keep_tgt <-
        train_targets %>%
        dplyr::summarize_at(dplyr::vars(dplyr::starts_with("class_")),sum) %>%
        which.min()

      train_targets <-
        train_targets %>% dplyr::select(class,names(keep_tgt))
    }

    tmp_target_columns <- grep("^class_",colnames(train_targets),value=TRUE)


  }else if(is.null(target_columns) & is.null(train_targets)) {
    # only data present, so take last col as target column

    train_targets <- train_data[,ncol(train_data)]
    train_data <- train_data[,1:(ncol(train_data)-1)]

    is_chr_trgt_vector <-
      is.character(train_targets) | is.factor(train_targets)

    is_int_trgt_vector <- all(train_targets %% 1 == 0)

    assertthat::assert_that(
      ## chr/factor vector with target classes
      is_chr_trgt_vector |
      ## int vector with target classes
      is_int_trgt_vector
    )

    assertthat::assert_that(unique(train_targets) > 1)

    train_targets <-
      train_targets %>%
      make.names() %>%
      as.factor() %>%
      data.table::as.data.table() %>%
      data.table::setnames("V1", "class") %>%
      mltools::one_hot(dropCols = FALSE)

    is_binary_class <-
      train_targets %>% dplyr::pull(class) %>% levels() %>% length() == 2

    if(is_binary_class){
    # if class only has 2 levels, drop the ohe column for the one which is larger
      keep_tgt <-
        train_targets %>%
        dplyr::summarize_at(dplyr::vars(dplyr::starts_with("class_")),sum) %>%
        which.min()

      train_targets <-
        train_targets %>% dplyr::select(class,names(keep_tgt))
    }

    tmp_target_columns <- grep("^class_",colnames(train_targets),value=TRUE)

  }else{
    # target cols available but not train targets, error!
    stop("`target_columns` was provided but not `train_targets`")
  }

  if(is.null(test_data) | train_only){
    # must be train_only OR split_data
    assertthat::assert_that(
      train_only | split_data,
      msg="Both `train_only` and `split_data` are set to `FALSE`, however no test data is provided."
    )

    if(split_data & !train_only){

      if(verbose>=2){
        message("'test_data' or 'test_targets' is NULL.")
        message("'train_only' is set to FALSE.")
        message("'train_data' will be partioned to create 'test_data'.")
      }
      # TODO: improve split method
      split_data <- make_train_test_split(
        train_d = train_data,
        train_targets = train_targets,
        targets=if(!is.null(target_columns)){target_columns}else{tmp_target_columns}
      )
      return(list(
          train_data=split_data$train_data,
          train_targets=split_data$train_targets,
          test_data=split_data$test_data,
          test_targets=split_data$test_targets,
          target_columns=if(!is.null(target_columns)){target_columns}else{tmp_target_columns}
      ))
    }
  } else {

    if(!is.null(target_columns) & !is.null(test_targets)){
      # DF + colnames
      assertthat::assert_that(all(target_columns %in% colnames(test_targets)))
      assertthat::assert_that(
        assertthat::are_equal(nrow(test_data), nrow(test_targets))
      )
      assertthat::assert_that(
        is.data.frame(test_targets) | is.matrix(test_targets)
      )
      # Make sure samples for the given target exist for testing (!=0)
      purrr::walk(target_columns, function(target){

        is_int_trgt_vector <- all(test_targets[, target] %% 1 == 0)

        ## int vector with target classes
        assertthat::assert_that(is_int_trgt_vector)

        samples_in_target <- sum(as.integer(test_targets[, target]))

        assertthat::assert_that(
          samples_in_target > 0,
          msg = paste0(
            "Target class: '", target, "' has 0 samples to train for.", "\n",
            "Make sure samples for the given target exist for testing."
          )
        )
        return(NULL)
      })
    }else if(is.null(target_columns) & !is.null(test_targets)){
      # vector w/ target classes
      assertthat::assert_that(
        assertthat::are_equal(nrow(test_data), length(test_targets))
      )

      is_chr_trgt_vector <-
        is.character(test_targets) | is.factor(test_targets)

      is_int_trgt_vector <- all(test_targets %% 1 == 0)

      assertthat::assert_that(
        ## chr/factor vector with target classes
        is_chr_trgt_vector |
          ## int vector with target classes
          is_int_trgt_vector
      )

      assertthat::assert_that(unique(test_targets) > 1)

      test_targets <-
        test_targets %>%
        make.names() %>%
        as.factor() %>%
        data.table::as.data.table() %>%
        data.table::setnames("V1", "class") %>%
        mltools::one_hot(dropCols = FALSE)

      is_binary_class <-
        test_targets %>% dplyr::pull(class) %>% levels() %>% length() == 2

      if(is_binary_class){
        # if class only has 2 levels, drop the ohe column for the one which is larger
        keep_tgt <-
          test_targets %>%
          dplyr::summarize_at(dplyr::vars(dplyr::starts_with("class_")),sum) %>%
          which.min()

        test_targets <-
          test_targets %>% dplyr::select(class,names(keep_tgt))
      }
    }else if(is.null(target_columns) & is.null(test_targets)) {
      # only data present, so take last col as target column
      test_targets <- test_data[,ncol(test_data)]
      test_data <- test_data[,1:(ncol(test_data)-1)]

      is_chr_trgt_vector <-
        is.character(test_targets) | is.factor(test_targets)

      is_int_trgt_vector <- all(test_targets %% 1 == 0)

      assertthat::assert_that(
        ## chr/factor vector with target classes
        is_chr_trgt_vector |
          ## int vector with target classes
          is_int_trgt_vector
      )

      assertthat::assert_that(unique(test_targets) > 1)

      test_targets <-
        test_targets %>%
        make.names() %>%
        as.factor() %>%
        data.table::as.data.table() %>%
        data.table::setnames("V1", "class") %>%
        mltools::one_hot(dropCols = FALSE)

      is_binary_class <-
        test_targets %>% dplyr::pull(class) %>% levels() %>% length() == 2

      if(is_binary_class){
        # if class only has 2 levels, drop the ohe column for the one which is larger
        keep_tgt <-
          test_targets %>%
          dplyr::summarize_at(dplyr::vars(dplyr::starts_with("class_")),sum) %>%
          which.min()

        test_targets <-
          test_targets %>% dplyr::select(class,names(keep_tgt))
      }
    }else{
      # target cols available but not test targets, error!
      stop("`target_columns` was provided but not `train_targets`")
    }
    # TODO: check if classes present in train overlap with classes in test?
    return(list(
        train_data=train_data,
        train_targets=train_targets,
        test_data=test_data,
        test_targets=test_targets,
        target_columns=if(!is.null(target_columns)){target_columns}else{tmp_target_columns}
    ))
  }

  return(
    list(
        train_data=train_data,
        train_targets=train_targets,
        test_data=NULL,
        test_targets=NULL,
        target_columns=if(!is.null(target_columns)){target_columns}else{tmp_target_columns}
    )
  )
}

