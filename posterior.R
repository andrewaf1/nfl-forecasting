accepted <- read.csv("accept_unif_best.csv")

plot(accepted$k, accepted$revert)

abline(v = mean(accepted$k))
abline(h = mean(accepted$revert))

#cols <- c("k", "revert", "mov_param_1", "mov_param_2", "hf_adj_1")

#scaled <- scale(accepted[cols])# / colMeans(accepted[cols])

#means <- rowMeans(scaled)

#hist(means)

#which.mean(means)