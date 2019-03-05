pvalues <- c(runif(100000), rbeta(1000, shape1=0.5, shape2=1000))

llr_values <- qchisq(1 - pvalues, df = 2)/2

llr_values2 <- 1000*log(qf(1 - pvalues, df1=1, df2=1000)/(1000) + 1)/2

library(ggplot2)

ggplot(data=data.frame(a=-log(pvalues), b=llr_values2),
       aes(x=a, y=b)) +
     theme_minimal() +
     xlab(expression(-ln(p))) +
     ylab(expression(LLR)) +
geom_point(size=0.2) +
ggsave("llr-vs-pvalue-contin.pdf", width=2, height=2)

ggplot(data=data.frame(a=-log(pvalues), b=llr_values),
       aes(x=a, y=b)) +
     theme_minimal() +
     xlab(expression(-ln(p))) +
     ylab(expression(LLR)) +
geom_point(size=0.2) +
ggsave("llr-vs-pvalue-binary.pdf", width=2, height=2)
