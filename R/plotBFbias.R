
plotBFbias <- function(y, sig, sig_prior_min=1e-2, sig_prior_max=1e2, Z=1.96){
	range_sig <- exp(seq(log(sig_prior_min), log(sig_prior_max), length.out = 40))
	print("Generating datapoints...");flush.console()
	for(x in seq_len(length(range_sig))) ans[x] <- BFbias(y, sig, sig_prior=range_sig[x], Z=Z)
	df <- as.data.frame(cbind(range_sig, ans))
	print("Plotting...");flush.console()
ggplot(df, aes(range_sig, ans)) + geom_smooth() + 
scale_x_log10(bquote(''*sigma[prior]~''), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
scale_y_log10("Bayes Factor: p( MixModel | data ) / p( Null | data )", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
geom_hline(yintercept=1, linetype="dashed", color="red") +
annotate("text", x=quantile(range_sig, 0.2), y=1.5, label="BF > 1: evidence of MixModel (bias)") + 
annotate("text", x=quantile(range_sig, 0.8), y=min(ans)*2, color="purple", label=paste("Minimum value of BF:", format(min(ans),scientific=T, digits=3)) )
	}

