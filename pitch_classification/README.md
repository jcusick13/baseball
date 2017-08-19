# Pitch Classification

This project looks to examine clustering techniques for classifying the type of pitch thrown by a given pitcher. To start, I'm using [PITCH f/x](https://en.wikipedia.org/wiki/PITCHf/x) data from Rick Porcello's 2016 season with the Boston Red Sox. 

Using the [pitchRx package](https://cran.r-project.org/web/packages/pitchRx/pitchRx.pdf), I can easily capture pitching data for a given timespan, but some initial work has to be done to subset down the PITCH f/x data to just Porcello.

`get_pitchfx.R` imports (previously downloaded) [Retrosheet Play-by-Play](http://www.retrosheet.org/game.htm) data and identifies the games and innings that Porcello pitched. From there, the corresponding PITCH f/x data is downloaded and combined into a single csv spreadsheet for later use.

Before any beginning any clustering, `cluster_pitches.R` performs some brief exploratory analysis to look at the dataset that's been built. 

<p align="center"> 
<img src="https://raw.githubusercontent.com/jcusick13/baseball/master/pitch_classification/images/speed.png" width="600">
</p>

Porcello is working with 5 seperate pitches, with his curveball having the slowest velocity and his four-seam fastball having the highest. It is interesting to note the two seperate velocity peaks within his changeup. Initially, it seems he either has two distinct flavors of a changeup, or that he isn't getting a good grip on them when he's throwing and the ball is coming out of his hand faster than he'd like. It will be interesting to see how his bimodal changeup is handled by the clustering analysis.

<p align="center">
<img src="https://raw.githubusercontent.com/jcusick13/baseball/master/pitch_classification/images/type.png" width="600">
</p>

When visualizing the dataset using just the horizonal and vertical movement of the baseball, we can see that some natural clusters begin to appear. Movement is measured in inches from the expected path of a theoretical pitch, devoid of any movement induced by spin. Therefore, this plot is not to say that all of his fastballs gain nearly a foot of elevation from where he released them, but rather that they have a higher trajectory than a similar pitch without any spin. This makes sense intuitively because fastballs are thrown with backspin, pushing the ball upward as it travels. Likewise, curveballs, thrown with forward spin, lose more elevation than a theoretical pitch of similar speed.

The clustering was accomplished with use of the [MClust package](https://cran.r-project.org/web/packages/mclust/mclust.pdf). MClust first chooses the number of clusters as well as the type of mixture model using Bayesian Information Criterion. From there, the chosen mixture model was fit to the pitch data using the EM algorithm.


<p align="center">
<img src="https://raw.githubusercontent.com/jcusick13/baseball/master/pitch_classification/images/freq_by_cluster.png" width="600">
</p>

Comparing the frequency of pitch types per cluster, we can see that the clustering algorithm has a strong agreement with the MLB assigned pitch type for most clusters, though a few still have some relative uncertainty. Clusters 3, 4, and 5 unambiguously align with changeups, curveballs, and sliders, respectively. Four-seam fastballs are well defined in clusters 1, 8, and 9, but the clustering wasn't quite sure of how to handle two-seamers. While we can loosely define cluster 2 as general fastballs, we're unable to name cluster 7 for now because of the strong disagreement with the MLB pitch types. However, we can safely ignore this cluster since it contains only 45 pitches, which accounts for 1.3% of the total dataset. 

All in all, the MClust package was pretty successful at creating a classification for Rick Porcello's pitches compared the MLB's official classifier. Possible extensions of this would be to have distinct clustering schemes for fastballs and for breaking balls. This could possibly help with differentiating between two and four seam fastballs.