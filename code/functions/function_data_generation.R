#' Data-Generating Mechanism
#'
#' This data-generating mechansism is specific to generating an ordinal response variable in a randomised controlled trial with two arms.
#' The allocation variable is supposed to be random and have roughly equal arm sizes.
#' The data-generating mechanism includes the choice of sample size, the number of categories and the probability distribution in the allocation variable. 
#' 
#'
#' @param n integer. The number of observations / The sample size.
#' @param K integer. The number of categories in the ordinal variable.
#' @param param character string or a vector of probabilities. The probability distribution of the treatment group and control group if not specified otherwise in param_control.
#' @param param_control character string or a vector of probabilities. The probability distribution of the control group.
#'
#' @return A data.table object with the allocation variable named 'treat_control' and the ordinal response variable 'y'.
#'
#' @export
#'
#' @examples
#' data_generation(n = 30, K = 3, param = "uniform", param_control = "linear increase")
#'
#'


data_generation <-
  function(n,
           K,
           param = "uniform",
           param_control = NULL) {
    # Packages ----
    if (!require('data.table')) {
      install.packages('data.table')
      library('data.table')
    }
    if (!require('dplyr')) {
      install.packages('dplyr')
      library('dplyr')
    }
    
    # Preliminary checks -----
    if (!is.numeric(n)) {
      stop("n (number of observations) has to be an integer!")
    }
    
    if (!is.numeric(K)) {
      stop("K (number of categories) has to be an integer!")
    }
    
    if (!all(is.element(
      param,
      c(
        "uniform",
        "linear increase",
        "increase of intermediate category",
        "geometric",
        "stroke"
      )
    ))) {
      if (!is.numeric(param)) {
        stop(
          "param (distribution parameters) must either be a custom vector of probabilities summing to 1 or it must contain one of the following expressions: uniform, linear increase, increase of intermediate category, geometric, stroke"
        )
      } else {
        if (length(param) != K) {
          stop(
            "The vector of probabilities must contain as many entries as categories K specified"
          )
        }
      }
    }
    
    if (!is.null(param_control)) {
      if (!all(is.element(
        param_control,
        c(
          "uniform",
          "linear increase",
          "increase of intermediate category",
          "geometric",
          "stroke"
        )
      ))) {
        if (!is.numeric(param_control)) {
          stop(
            "param (distribution parameters) must either be a custom vector of probabilities summing to 1 or it must contain one of the following expressions: uniform, linear increase, increase of intermediate category, geometric, stroke"
          )
        } else {
          if (length(param_control) != K) {
            stop(
              "The vector of probabilities must contain as many entries as categories K specified"
            )
          }
        }
      }
    }
    
    
    # Figure out the probabilities -----
    # param
    if (all(is.element(
      param,
      c(
        "uniform",
        "linear increase",
        "increase of intermediate category",
        "geometric",
        "stroke"
      )
    ))) {
      if (param == "uniform") {
        prob = rep(1 / K, K)
      } else if (param == "linear increase") {
        prob <- c()
        for (i in 1:K) {
          prob[i] = i / sum(1:K)
        }
      } else if (param == "increase of intermediate category") {
        prob <- rep(1 / sum(1:K), K)
        prob[ceiling(K / 2)] <- 1 - sum(prob[-1])
      } else if (param == "geometric") {
        r = 2
        a <- 100 / sapply(list(0:(K - 1)), function(x) {
          sum(2 ^ x)
        })
        prob <- unlist(lapply(0:(K - 1), function(x) {
          sum(a * 2 ^ x)
        })) / 100
      } else if (param == "stroke" && K == 7) {
        prob <- c(0.15, 0.21, 0.18, 0.16, 0.13, 0.07, 0.1)
      } else if (param == "stroke" && K == 3) {
        prob <- c(0.76, 0.14, 0.1)
      } else if (param == "stroke" && K == 5) {
        prob <- c(1447, 32, 15, 33, 13) / 1540
      } else if (param == "stroke" && K == 9) {
        prob <- c(1447, 32, 5, 10, 12, 10, 6, 5, 13) / 1540
      }
    } else {
      prob = param
    }
    
    # param_control
    if (all(is.element(
      param_control,
      c(
        "uniform",
        "linear increase",
        "increase of intermediate category",
        "geometric",
        "stroke"
      )
    ))) {
      if (!is.null(param_control)) {
        if (param_control == "uniform") {
          prob_control = rep(1 / K, K)
        } else if (param_control == "linear increase") {
          prob_control <- c()
          for (i in 1:K) {
            prob_control[i] = i / sum(1:K)
          }
        } else if (param_control == "increase of intermediate category") {
          prob_control <- rep(1 / sum(1:K), K)
          prob_control[ceiling(K / 2)] <- 1 - sum(prob_control[-1])
        } else if (param_control == "geometric") {
          r = 2
          a <- 100 / sapply(list(0:(K - 1)), function(x) {
            sum(2 ^ x)
          })
          prob_control <-
            unlist(lapply(0:(K - 1), function(x) {
              sum(a * 2 ^ x)
            })) / 100
        } else if (param_control == "stroke" && K == 7) {
          prob_control <- c(0.07, 0.1, 0.12, 0.15, 0.24, 0.12, 0.19)
        } else if (param_control == "stroke" && K == 3) {
          prob_control <- c(0.75, 0.15, 0.1)
        } else if (param_control == "stroke" && K == 5) {
          prob_control <- c(1425, 48, 18, 32, 7) / 1530
        } else if (param_control == "stroke" && K == 9) {
          prob_control <- c(1425, 48, 4, 14, 11, 12, 8, 1, 7) / 1530
        }
      }
    } else {
      prob_control = param_control
    }
    
    
    # 1. Allocating treatment or control -----
    data <-
      sample(c(0:1), n, replace = TRUE) %>% list(treat_control = .) %>% setDT
    # treatment: 1, control: 0
    
    # 2. Allocating answers to subjects respectively -----
    data[, y := sample(1:K, n, replace = TRUE, prob = prob)]
    
    # second parameter for control group
    if (!is.null(param_control)) {
      data[treat_control %in% 0, y := sample(1:K,
                                             nrow(data[treat_control %in% 0]),
                                             replace = TRUE,
                                             prob = prob_control)]
      rm(prob_control)
    }
    
    # Return dataset -----
    rm(prob)
    
    return(data)
  }
