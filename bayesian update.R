library(LaplacesDemon)


#forecast outcome <- c(forecast prob of B, forecast prob of N, forecast prob of A)
fpB <- 0.3
fpN <- 0.4
fpA <- 0.3
forecast <- c(fpB, fpN, fpA)

##contigency table
#Co__ means Contigency outcome = __
CoB <- c(80, 10, 10)
CoN <- c(10, 80, 10)
CoA <- c(10, 10, 80)
contigency <- rbind(CoB, CoN, CoA)
colnames(contigency) <- c("CfB", "CfN", "CfA")
contigency 

##prior
#prior = CWB historical forecast outcome  
##historical probability of forecasts
HpB <- 0.3
HpN <- 0.4
HpA <- 0.3
histprob <- c(HpB,HpN,HpA)
histprob

##likelihood function
#PLfo = P(f=k|o=n) = h(k,n) / h(all k,n)
#Probability of likelihoods
PLBB <- contigency["CoB","CfB"] / sum(contigency["CoB",])
PLBN <- contigency["CoB","CfN"] / sum(contigency["CoB",])
PLBA <- contigency["CoB","CfA"] / sum(contigency["CoB",])
PLNB <- contigency["CoN","CfB"] / sum(contigency["CoN",])
PLNN <- contigency["CoN","CfN"] / sum(contigency["CoN",])
PLNA <- contigency["CoN","CfA"] / sum(contigency["CoN",])
PLAB <- contigency["CoA","CfB"] / sum(contigency["CoA",])
PLAN <- contigency["CoA","CfN"] / sum(contigency["CoA",])
PLAA <- contigency["CoA","CfA"] / sum(contigency["CoA",])
#Lo__ means Likelihood outcome = __
LoB <- c(PLBB,PLBN,PLBA)
LoN <- c(PLNB,PLNN,PLNA)
LoA <- c(PLAB,PLAN,PLAA)
#combine into a likelihood matrix
likelihood <- rbind(LoB,LoN,LoA)
colnames(likelihood) <- c("LfB","LfN","LfA")
likelihood

##Bayes Theorem
PostB <- BayesTheorem(histprob, likelihood["LoB",])
PostN <- BayesTheorem(histprob, likelihood["LoN",])
PostA <- BayesTheorem(histprob, likelihood["LoA",])
#combine into a posterior matrix
posterior <- rbind(PostB, PostN, PostA)
colnames(posterior) <- c("fB","fN","fA")
rownames(posterior) <- c("oB",'oN',"oA")
posterior


##final probability (updated forecast)
PB <- posterior["oB","fB"]*fpB + posterior["oB","fN"]*fpN + posterior["oB","fA"]*fpA
PN <- posterior["oN","fB"]*fpB + posterior["oN","fN"]*fpN + posterior["oN","fA"]*fpA
PA <- posterior["oA","fB"]*fpB + posterior["oA","fN"]*fpN + posterior["oA","fA"]*fpA
PB
PN
PA

NEW_forecast <- c(PB,PN,PA)
NEW_forecast
sum(NEW_forecast)



