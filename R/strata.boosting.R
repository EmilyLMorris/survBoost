# FUNCTION FOR LOOKING AT POSSIBLE VARIABLE STRATA

#' Stratification function
#'
#' This function assists in evaluating whether the supplied variable is useful for stratification when fitting a cox proportional hazards model.
#' @param x variable that may be used for stratification, can be categorical or continuous.
#' @param survival.time vector of survival time corresponding to input vector x.
#' @param split specifies how to split a continuous variable. Default is median value.
#' @return Generates a plot and table. Table displays the quartiles of the groups of x. A boxplot is also generated to display the distributions of the groups in x visually.
#' @keywords gradient boosting
#' @export
#' @examples
#' data <- simulate_survival_cox(true_beta=c(1,1,1,1,1,0,0,0,0,0))
#' strata.boosting(data$strata_idx, data$time)
#'
strata.boosting <- function(x, survival.time, split="median"){
  library(ggplot2)
  library(plyr)
  data <- data.frame(x, survival.time)

  give.n <- function(x){
    return(c(y = mean(x), label = length(x)))
  }

  if(is.vector(x) & !is.list(x)){ # check that x is a vector - just one variable
    if(is.factor(x)){
      p <- ggplot(data, aes(x=x, y=survival.time)) + geom_boxplot() + theme_bw()+
        stat_summary(fun.data = give.n, geom = "text") + ylab("Survival Time")  + xlab(NULL)
      print(p)

      # print summary table
      ddply(data, .(x), summarise,  Min=quantile(survival.time, 0), Q1=quantile(survival.time, 0.25),
            Median=quantile(survival.time, 0.5), Q3=quantile(survival.time, 0.75), Max=quantile(survival.time, 1))
    }
    else if(length(unique(x))<10){ # limit is 10 strata
      p <- ggplot(data, aes(x=as.factor(x), y=survival.time)) + geom_boxplot() + theme_bw()+
        stat_summary(fun.data = give.n, geom = "text") + ylab("Survival Time") + xlab(NULL)
      print(p)

      # print summary table of quartiles
      ddply(data, .(as.factor(x)), summarise,  Min=quantile(survival.time, 0), Q1=quantile(survival.time, 0.25),
            Median=quantile(survival.time, 0.5), Q3=quantile(survival.time, 0.75), Max=quantile(survival.time, 1))

    }
    else{ # continuous case
      if(split=="median"){
        med <- median(data$x)
        x.split <- rep(0,length(x))
        x.split[which(x > med)] <- 1
        p <- ggplot(data, aes(x=as.factor(x.split), y=survival.time)) + geom_boxplot() + theme_bw()+
          stat_summary(fun.data = give.n, geom = "text") + ylab("Survival Time") + xlab(NULL) +
          scale_x_discrete(labels=c("0" = "< Median", "1" = "> Median"))
        print(p)

        ddply(data, .(as.factor(x.split)), summarise,  Min=quantile(survival.time, 0), Q1=quantile(survival.time, 0.25),
              Median=quantile(survival.time, 0.5), Q3=quantile(survival.time, 0.75), Max=quantile(survival.time, 1))
      }
    }
  }
  # suppose that x is a vector of variables - may want a combination

}
