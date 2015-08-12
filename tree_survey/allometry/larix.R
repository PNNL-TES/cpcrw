# Read the Bond-Lamberty (2002) raw Larix data and write out Alexander (2012)-type table

d <- read.csv("Bond-Lamberty_2002_Larix_raw.csv")
d$Foliage <- with(d, OldFol+NewFol)
d$Total <- with(d, Foliage+OldBranch+NewBranch+Stem)

alexander_terms <- c("Total biomass", "Stemwood/bark", "Foliage")
bl_terms <- c("Total", "Stem", "Foliage")

results <- data.frame(indepvar=NA, Component=NA, a=NA, b=NA, R2=NA, P001=NA, Obs=NA)
for(indepvar in c("DBH", "D0")) {
    
    for(depvar in 1:3) {
        y <- d[, bl_terms[depvar]]
        x <- d[, indepvar]
        cat("x=", indepvar, "y=", bl_terms[depvar], "\n")
        m <- lm(log(y) ~ log(x))
        newrow <- c(indepvar, alexander_terms[depvar], coefficients(m), summary(m)$adj.r.squared, summary(m)$coefficients[2,4] < 0.001, length(summary(m)$residuals))
        results <- rbind(results, newrow)
    }
}

results <- na.omit(results)


# Reshape

library(reshape2)
d <- melt(results,id.vars=1:2)
d <- dcast(d, Component ~ indepvar+variable)
d$Species <- "LALA"
print(d)
write.csv(d, "Bond-Lamberty_2002_Larix.csv", row.names=FALSE)

