#Factor Analysis steps
setwd("~/helloR") #set directory
qa = read.csv("questionnaire_coded.csv") #read csv file
cm <- cor(qa) #calculate correlation matrix from questionnaire answers
pca <- princomp(cm, scores=TRUE, cor=TRUE) #principal component analysis from correlation matrix
summary(pca) #show "Importance of components", eigenvalue on top, standard deviation, cumulative explanative power
screeplot(pca, type="line", main="ScreePlot") #draws a graph to help deciding the number of factors
ve <- cbind(qa) #make vectors out of each line of answers in the table
fa2 <- factanal(ve, factor=2, rotation="varimax", scores="regression") #run analysis with 2 factors
fa2 #show the results
fa4 <- factanal(ve, factor=4, rotation="varimax", scores="regression") #with 4 factors
fa4

#Other learnings
#tutorial video to follow: https://www.youtube.com/watch?v=Od8gfNOOS9o
View(qa) #show csv file separately
cor(qa$ag, qa$fi) #calculate correlation between two questions. Results in [1] 0.02818612
plot(pca) #draws basic graphs for any purpose, eigenvalues for this
loadings(pca) #shows how components are different from each other. A way to think of what they could mean
pca$scores[1:5,] #shows how components score values in each question
biplot(pca) #draws a visual presentation of sorts to 2-D
fa4$loadings #show only the loadings table
fa4$uniquenesses #show how much uniqueness there was in the answers based on each question
