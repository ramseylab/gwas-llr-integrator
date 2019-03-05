grasp_df <- read.table("grasp-sig-SNPs.txt", sep="\t",
                       header=TRUE,
                       stringsAsFactors=FALSE)

gtex_df <- read.table("gtex-min-tss-split.txt", sep="\t",
                      header=TRUE,
                      stringsAsFactors=FALSE)

                     
names(grasp_df) <- c("rsid", "chrom", "pos")
gtex_df$key <- paste(gtex_df$chrom, gtex_df$pos, sep="-")
grasp_df$key <- paste(grasp_df$chrom, grasp_df$pos, sep="-")

grasp_df <- subset(grasp_df,
                   chrom != "MT" & chrom != "Y")

merged_df <- merge(x=gtex_df, y=grasp_df,
                   by.x="key", by.y="key", all.x=TRUE, all.y=FALSE)

merged_df$nlpv <- -log(merged_df$pvalue)
merged_df$pos.y <- NULL
merged_df$chrom.y <- NULL
names(merged_df) <- c("key", "chrom", "pos", "tssdist", "pvalue", "rsid", "nlpv")
merged_df$is_grasp_snp <- factor(! is.na(merged_df$rsid))

merged_df_isgrasp <- subset(merged_df, TRUE == is_grasp_snp)
merged_df_notgrasp <- subset(merged_df, FALSE == is_grasp_snp)
orig_breaks <- hist(merged_df_isgrasp$nlpv, plot=FALSE)$breaks
breaks <- orig_breaks[c(1:9, length(orig_breaks))]
bin_probs_isgrasp <- hist(merged_df_isgrasp$nlpv, plot=FALSE, breaks=breaks)$counts / nrow(merged_df_isgrasp)
bin_probs_notgrasp <- hist(merged_df_notgrasp$nlpv, plot=FALSE, breaks=breaks)$counts / nrow(merged_df_notgrasp)
llr <- log(bin_probs_isgrasp / bin_probs_notgrasp)
bin_centers <- 0.5*(breaks[1:(length(breaks)-1)] + breaks[2:length(breaks)])
res_to_plot <- data.frame(bin_centers, llr)
plot(bin_centers, llr)

library(ggplot2)
ggplot(data=res_to_plot,
       aes(x=bin_centers, y=llr)) +
    theme_classic(base_size=18) +
    geom_col(width=c(rep(4.5, 8), 24)) +
    xlab(expression(-ln(p^e))) +
    ylab("LLR") +         
    ggsave("llr-eqtl.pdf", width=3, height=3)

breaks2 <- log(10)*c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 10, 15, 20, 25, 30, 35, 40, 65)

bin_probs_isgrasp <- hist(merged_df_isgrasp$nlpv, plot=FALSE, breaks=breaks2)$counts / nrow(merged_df_isgrasp)
bin_probs_notgrasp <- hist(merged_df_notgrasp$nlpv, plot=FALSE, breaks=breaks2)$counts / nrow(merged_df_notgrasp)
llr <- log(bin_probs_isgrasp / bin_probs_notgrasp)
bin_centers <- 0.5*(breaks2[1:(length(breaks2)-1)] + breaks2[2:length(breaks2)])
res_to_plot2 <- data.frame(bin_centers, llr)
plot(bin_centers, llr)

library(ggplot2)
ggplot(data=res_to_plot2,
       aes(x=bin_centers, y=llr)) +
    theme_classic(base_size=18) +
    geom_point() +
    xlab(expression(-ln(p^e))) +
    ylab("LLR") +         
    ggsave("llr-eqtl2.pdf", width=3, height=3)

print(lm(data=subset(res_to_plot2, bin_centers < -log(10^{-4})), llr~bin_centers))
print(lm(data=subset(res_to_plot2, bin_centers > -log(10^{-4})), llr~bin_centers))



