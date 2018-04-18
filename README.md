# Project 4: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Spring 2018

+ Team #9
+ Projec title: Collaborative Filtering
+ Team members
	+ Chen, Ziyu [zc2393@columbia.edu]()
	+ Kang, Yuhao [yk2758@columbia.edu]()
	+ Lin, Yanjun [yl3829@columbia.edu]()
	+ Liu, Fangbing [fl2476@columbia.edu]()
+ Project summary: In this project, we used memory-based algorithm and model-based algorithm to do collaborative filtering. For memory-based algorithm, we use best-n estimator to select neighbours and use combination of different similarities: Pearson correlation, Mean-square-difference, Simrank similarity(only for EachMovie data) and variance weight to do collaborative filtering. We use cluster model (only for Microsoft Web Data) as model-based algorithm. For evaluation, we use ranked scoring to evaluate Microsoft Web Data and use MAE and ROC_4 to evaluate EachMovie data.

 The following shows our result:
 
 
+ Presentation slides can be found here: [Googld Slide](https://docs.google.com/presentation/d/1_qWU0l8XnkMMdqb7Q2sZuy30lrrRvheV3Rdc0Ui7-k8/edit#slide=id.g3848589a50_0_35)

  
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md))
  
  + Chen, Ziyu: Built SimRank model. Organized the main.rmd file.
  + Kang, Yuhao: Calculated variance weighting. Created selecting neighbours (Best-n estimator) and prediction. Organized the main.rmd file.
  + Lin, Yanjun：Processed orginal data. Built Cluster Model (EM algorithm). Created rank score evaluation. Organized main.rmd file. Prepared the presentation. Combined variance wighting with MSD and Correlation, and improved code efficiency. Improved SimRank by seperating the data into positive and negative connections.
  + Liu, Fangbing：Processed original data. Calculated similarity weight (Pearson Correlation and Mean-Square-Difference). Created MAE and ROC evaluation. Organized the main.rmd file. Wrote the summary page on github.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
