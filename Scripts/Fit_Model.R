fit<- lm(ML_RATING~ drama+romance+thriller+comedy+`sci-fi`+`adventure`,
         data = movie_new)
summary(fit)
hist(movie_new$STARTYEAR)
mean(movie_new$ML_RATING-(movie_new$AVERAGERATING/2))
plot(movie_new$REVENUElog~movie_new$AVERAGERATING)
