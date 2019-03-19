# GSWproject
This project aims to set a prediction model to forecast GSW final game score according to half-time box score. 
The whole process is automated and need to be luanch on a virtual machine.

GSW.ipynb is the code for scraping data, data pipeline, and model in Python.
GSW_point_diff_prediction_using_halftime.R is for researching on data distribution and model selecting.


Following is the flow of the project.

1. Pull play-by-play text data of each game from basketball reference.
2. Clean text data and transform them into customized box score such as first quarter box score or half-time box score.
3. Train a LASSO regression model using half-time boxscore.
4. Set schedule for pulling real time, half time box score from Fox sports. 
5. Predict final score for playing game.
6. Update dataframe store in Google cloud.
7. Wait till end of the game, pull final score and retrain the model.
8. Loop step 4 to step 7 until the season ends.
