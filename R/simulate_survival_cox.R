#############################################
### function for simulating survival data ###
#############################################

require(mvtnorm)

#' Simulating survival data
#'
#' This function allows you to simulate stratified survival data.
#' @param true_beta Contains true parameter values to simulate from.
#' @param base_hazard
#' @param base_hazard_scale
#' @param base_hazard_shape
#' @param num_strata
#' @param input_strata_size
#' @param z_matrix
#' @param cov_structure
#' @param block_size
#' @param rho
#' @param censor_dist
#' @param censor_const
#' @param tau
#' @param normalized
#' @return a matrix with survival time (time), event indicator (delta), stratification variable (strata_idx), a vector for each variable specified by the true_beta.
#' @keywords gradient boosting
#' @export
#' @examples
#' toyData <- simulate_survival_cox(true_beta=c(1,1,1,1,1,0,0,0,0,0), base_hazard="weibull", base_hazard_scale=rep(1,5), base_hazard_shape=rep(2,5), num_strata=5, input_strata_size=100, cov_structure="diag", block_size=2, rho=0.3, censor_dist="unif", censor_const=5, tau=Inf, normalized=F)
#' any(duplicated(toyData$time))
#' z <- as.matrix(toyData[,-c(1,2,3)])
#'
#'
simulate_survival_cox <- function(true_beta, base_hazard="auto", base_hazard_scale=NULL, base_hazard_shape=NULL,
                                  num_strata=10, input_strata_size=50, z_matrix=NULL, cov_structure="diag",
                                  block_size=1, rho=NULL, censor_dist="unif", censor_const=5, tau=Inf, normalized=F)
{
	p <- length(true_beta)
	if(length(input_strata_size) == 1)
	{strata_size <- rpois(num_strata, lambda=input_strata_size)}
	else
	{
		if(length(input_strata_size) == num_strata)
		{
			strata_size <- input_strata_size
		}
		else
		{
			stop("Invalid input for 'input_strata_size'!")
		}
	}
	n <- sum(strata_size)

	## covariance structure ##

	if(is.null(z_matrix))
	{
	cov_z <- matrix(0, ncol=p, nrow=p)
	if(cov_structure == "diag") { cov_z <- diag(p) }
	else if(cov_structure == "ar")
	{
		if(!(p%%block_size == 0)) stop("Length of coefficients is not a multiple of block size.")
		if(is.null(rho)) stop("Correlation parameter is missing.")
		k <- p/block_size # number of blocks
		cov_block <- matrix(0, ncol=block_size, nrow=block_size)
		times <- 1:block_size
		H <- abs(outer(times, times, "-"))
		cov_block <- rho^H
		cov_z <- kronecker(diag(k), cov_block)
	}
	else if(cov_structure == "cs")
	{
		if(!(p%%block_size == 0)) stop("Length of coefficients is not a multiple of block size.")
		if(is.null(rho)) stop("Correlation parameter is missing.")
		k <- p/block_size # number of blocks
		cov_block <- rho*outer(rep(1,block_size), rep(1,block_size)) + (1-rho)*diag(block_size)
		cov_z <- kronecker(diag(k), cov_block)
	}
	else stop("Invalid parameter for covariance structure.")
	}
	## covariate matrix ##

	normalize = function(x){
        y = (x-mean(x))/sqrt(sum((x-mean(x))^2))
        return(y)
	} # a function to normalize a vector to have l2 norm to be 1
	if(is.null(z_matrix))
	{z <- mvtnorm::rmvnorm(n, mean=rep(0,p), sigma=cov_z)}
	else
	{
		if(ncol(z_matrix) != p)
		{stop("Number of columns in 'z_matrix' must be identical to the dimension of 'true_beta'!")}
		else {z <- z_matrix}
	}
	if(normalized) { z <- apply(z, 2, normalize) } # if "normalized=T", normalize every column of matrix z

	## baseline hazard ##

	if(base_hazard == "auto") # randomly generated for each strata as distinct exponential r.v.'s
	{
		scale <- exp(rnorm(num_strata, mean=0, sd=0.4))
		scale_subject <- rep(scale, strata_size) # dim = n
		shape_subject <- rep(1, n)
	}
	else if(base_hazard == "weibull")
	{
		if(is.null(base_hazard_scale) | is.null(base_hazard_shape))
		stop("Scale or shape parameters for baseline hazards are missing.")
		else if((length(base_hazard_scale) != length(base_hazard_shape)) | (length(base_hazard_scale) != num_strata) | (length(base_hazard_shape) != num_strata))
		stop("Please make sure the lengths of both shape and scale parameters for baseline hazards are equal to the number of strata.")
		else
		{
			scale_subject <- rep(base_hazard_scale, strata_size)
			shape_subject <- rep(base_hazard_shape, strata_size)
		}
	}
	else
	{ stop("Invalid input parameters for baseline hazards.") }

	strata_idx <- rep(c(1:num_strata), strata_size)

	## generate latent event time ##

	uvec <- runif(n, 0, 1)
	latent_t <- (-log(uvec)/(scale_subject*as.vector(exp(z%*%true_beta))))^{1/shape_subject}

	## generate censoring time ##

	if(censor_dist == "unif")
	{
		if(censor_const <= 1)
		{
			stop("In uniform censoring distribution, please specify consor_dist to be larger than 1.")
		}
		latent_c <- runif(n, 1, censor_const)
	}
	else if(censor_dist == "exp")
	{
		latent_c <- rexp(n, rate=censor_const)
	}
	else stop("Invalid parameter for censoring distribution.")
	pre_censoring <- ifelse(latent_c < tau, latent_c, tau)

	## generate observed data ##

	delta <- as.numeric(pre_censoring > latent_t)
	time <- latent_t*(delta==1) + pre_censoring*(delta==0)
	od_time <- order(time)
	delta <- delta[od_time]
	z <- z[od_time,]
	strata_idx <- strata_idx[od_time]
	time <- time[od_time]

	## final product is a data.frame ##

	final_product <- matrix(NA, nrow=n, ncol=3+p)
	final_product[,1] <- time
	final_product[,2] <- delta
	final_product[,3] <- strata_idx
	final_product[,c(4:(3+p))] <- z
	final_product <- (as.data.frame(final_product))
	names(final_product) <- c("time", "delta", "strata_idx", paste("V", 1:p, sep=""))
	return(final_product)
}

