# GSWproject
This project aims to predict final game score according to half-time box score. 

Following is the flow of the project.

1. Pull play-by-play text data of each game from basketball reference.
2. Clean text data and transform them into customized box score such as first quarter box score or half-time box score.
3. Train a LASSO regression model using half-time boxscore.
4. Set schedule for pulling real time, half time box score from Fox sports. 
5. Predict final score for playing game.
6. Update dataframe store in Google cloud.
7. Wait till end of the game, pull final score and retrain the model.
8. Loop step 4 to step 7 until the season ends.
