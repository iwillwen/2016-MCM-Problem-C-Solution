# 2016 MCM Problem C Solution

This our work on 2016 MCM Problem C, which is including a lot of machine learning methods such as SVM(Support Vector Machine), ANN(Artificial neural network) and K-Means.

---
## Summary

The paper bases on the data which the IPEDS (The Integrated Postsecondary Education Data System) provided to the public and the data from 7804 colleges and universities in the United States.

Our task is to help the Goodgrant Foundation to develop a model to determine an optimal investment strategy that identifies the schools, the investment amount per school, the return on that investment, and the time duration that the organization’s money should be provided to have the highest likelihood of producing a strong positive effect on student performance.

The paper proposes one model, three algorithms and two formulas. The model set up under the machine learning technology including Support Vector Machines (SVM), Artificial Neural Network (ANN) and K-means algorithm. The two formulas are a way to calculate the weight is that we thought the better way to find the balance between the level of the salary and the better way to distribute the optimal investment strategy.

According to some data such as SAT scores, ACT scores and the salary level the graduates got, we made a formula to calculate the entire scores of the schools and aggregate them to 5 clusters which also means5 levels just like the QR Ranking Stars.

The rates of every cluster are based on a formula we created.

<center>![Formula 1](http://ww3.sinaimg.cn/large/7287333fgw1f0kq9setdvj208c01mdfp.jpg)</center>

- ***score<sub>max</sub>*** and ***score<sub>min</sub>*** is the maximal score and the minimal score of the 5 cluster centers. 

<center>![Formula 2](http://ww4.sinaimg.cn/large/7287333fgw1f0kqba0w6oj208c01pglh.jpg)</center>

- ***i*** is the sorted index of the clusters, like 1, 2, 3.

<center>![Formula 3](http://ww4.sinaimg.cn/large/7287333fgw1f0kqcsl6nzj204a01m743.jpg)</center>

- ***r<sub>i</sub>*** is the ***i<sup>th</sup>*** cluster’s rate of investment.

The result of our model can be shown by the image as following.

<center>![Result](http://ww2.sinaimg.cn/large/7287333fgw1f0kqgxu7qlj20dw09qdgg.jpg)</center>

Finally, the flow chart of algorithm in our mode, strengths and weaknesses and the improvement of model are given.