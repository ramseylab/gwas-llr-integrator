ck2_scores <- read.table("ck2_pred_scores.tsv",
                         header=TRUE,
                         sep="\t",
                         stringsAsFactors=FALSE)

grasp_data <- read.table("grasp-sig-SNPs.txt",
                         header=TRUE,
                         sep="\t",
                         stringsAsFactors=FALSE)

                         
names(grasp_data) <- c("rsid", "chr", "pos")
grasp_data$rsid <- paste("rs", grasp_data$rsid, sep="")

ck2_data <- cbind(ck2_scores[,1:2], apply(ck2_scores[,3:13], 1, mean))
names(ck2_data) <- c("rsid", "label", "score")

merged_data <- merge(x=ck2_data, y=grasp_data, by.x="rsid", by.y="rsid", all.x=TRUE, all.y=FALSE)

logit <- function(x) {
    log2(x/(1-x))
}

breaks <- hist(logit(merged_data$score), plot=FALSE)$breaks

merged_data_rsnp <- subset(merged_data, label==1)
merged_data_notrsnp <- subset(merged_data, label==0)

probs_rsnp <- hist(logit(merged_data_rsnp$score), breaks=breaks, plot=FALSE)$counts / nrow(merged_data_rsnp)

probs_notrsnp <- hist(logit(merged_data_notrsnp$score), breaks=breaks, plot=FALSE)$counts / nrow(merged_data_notrsnp)

centers <- 0.5*(breaks[1:(length(breaks)-1)] + breaks[2:length(breaks)])
dev.new()
plot(centers, log(probs_rsnp/probs_notrsnp))

merged_data$isgrasp <- ! is.na(merged_data$pos)
merged_data$isrsnp <- merged_data$label==1

ct <- table(merged_data[,6:7])

print(fisher.test(ct))

llrhr <- log(probs_rsnp/probs_notrsnp)
llrhrc0 <- (ct[1,2]/(ct[1,2]+ct[2,2]))/(ct[1,1]/(ct[1,1]+ct[2,1]))
llrhc <- llrhr - log( (1 + exp(llrhrc0)*exp(llrhr))/(1 + exp(llrhr)) )

library(ggplot2)

data_for_plot <- rbind(data.frame(logit_pa=centers,
                                  LLR=llrhr,
                                  type="LLR_Hr")[1:9,],
                       data.frame(logit_pa=centers,
                                  LLR=llrhc,
                                  type="LLR_Hc")[1:9,])

ggplot(data=data_for_plot,
       aes(x=logit_pa, y=LLR)) +
    geom_point(aes(colour=type)) +
    theme_minimal() +
    scale_colour_grey() +
    xlab(expression(logit(p^a))) +
    ggsave("llrpa.pdf", width=3, height=2)

