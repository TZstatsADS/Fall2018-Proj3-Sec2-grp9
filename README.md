# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2018

+ Team 9
+ Team members
  +Bai, Ruoxi rb3313@columbia.edu
  +Jin, Xin xj2215@columbia.edu
  +Wang, Yan yw3177@columbia.edu
  +Wang, Yujia yw3085@columbia.edu
  +Zhong, Chenzhong cz2486@columbia.edu
+ Project summary: Tasked with increasing picture resolution in this project, we trained a GBM model as the baseline , and an Xgboost with modified data and parameters as the improvement, successfully increasing the PSNR.
	
**Contribution statement**:  Chenzhong Zhong did feature extraction, training, super resolution, and PSNR part of both models. Ruoxi Bai developed the Xgboost model as an improvement, including tuning parameters with cross validation. Xin Jin did super resolution for both models, and tuning parameters with cross validation for the baseline model. Yujia Wang did super resolution part for both models, and cross validation for the Xgboost model. Yan Wang partipated in group meetings.


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
