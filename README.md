# Google-Cloud-NCAA-ML-Competition-2019-Mens-Womens
This is a project I worked on for school and not as a part of the competition

Women's:
https://www.kaggle.com/c/womens-machine-learning-competition-2019

Men's:
https://www.kaggle.com/c/mens-machine-learning-competition-2019



The sixth annual Kaggle-backed March Madness competition is underway! Another year, another chance to anticipate the upsets, call the probabilities, and put your bracketology skills to the leaderboard test. Kagglers will join the millions of fans who attempt to forecast the outcomes of March Madness during this year's NCAA Division I Men’s and Women’s Basketball Championships. But unlike most fans, you will pick your bracket using a combination of NCAA’s historical data and your computing power, while the ground truth unfolds on national television. 

In the first stage of the competition, Kagglers will rely on results of past tournaments to build and test models. We encourage you to post any useful external data as a dataset. In the second stage, competitors will forecast outcomes of all possible matchups in the 2019 NCAA Division I Men’s and Women’s Basketball Championships. You don't need to participate in the first stage to enter the second. The first stage exists to incentivize model building and provide a means to score predictions. The real competition is forecasting the 2019 results.

Submissions are scored on the log loss:

LogLoss=−1n∑i=1n[yilog(ŷ i)+(1−yi)log(1−ŷ i)],

where

    n is the number of games played
    ŷi is the predicted probability of team 1 beating team 2
    yi is 1 if team 1 wins, 0 if team 2 wins
    log() is the natural (base e) logarithm

The use of the logarithm provides extreme punishments for being both confident and wrong. In the worst possible case, a prediction that something is true when it is actually false will add an infinite amount to your error score. In order to prevent this, predictions are bounded away from the extremes by a small value.


I utilized a variety of different approaches and worked from various kernels provided from pervious years' competitions.  I utilized both R and Python but did not actively compete in this year's competition.  This was a great learning experience for me and helped me bolster my programming skills and knowledge of ML algorithms.

Kernels I utilized:
raddar, 2018 Women's winner: https://github.com/dbrait/ncaa_women_2018
Virasat Khan's EDA: https://www.kaggle.com/kvirasat/women-s-ncaa-data-visualization
Enrique Perez Herrero's starter R kernel: https://www.kaggle.com/enrique1500/basic-starter-kernel-translated-into-r/code
KevinPan's minial elo R code: https://www.kaggle.com/kevinpan/minimal-code-for-fivethirtyeight-s-elo-predictions/log
Conor Dewey's 2018 approach which utilized FiveThirtyEight's elo code: https://github.com/conordewey3/Kaggle-March-Madness-Machine-Learning-2018

I tried to take elements from everyone's approaches to create my own unique approach.
