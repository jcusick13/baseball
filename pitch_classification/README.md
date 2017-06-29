# Pitch Classification

This project looks to examine different clustering techniques for classifying the type of pitch thrown by a given pitcher. To start, I'm using [PITCH f/x](https://en.wikipedia.org/wiki/PITCHf/x) data from Rick Porcello's 2016 season with the Boston Red Sox. 

Using the [pitchRx package](https://cran.r-project.org/web/packages/pitchRx/pitchRx.pdf), I can easily capture pitching data for a given timespan, but some initial work has to be done to subset down the PITCH f/x data to just Porcello.

`get_pitchfx.R` imports (previously downloaded) [Retrosheet Play-by-Play](http://www.retrosheet.org/game.htm) data and identifies the games and innings that Porcello pitched. From there, the corresponding PITCH f/x data is downloaded and combined into a single csv spreadsheet for later use.

Before any beginning any clustering, `cluster_pitches.R` performs some brief exploratory analysis to look at the dataset I've built. 

![alt text](https://raw.githubusercontent.com/jcusick13/baseball/master/pitch_classification/images/speed.png "Speed by pitch type")

Porcello is working with 5 seperate pitches; his curveball having the slowest velocity and his four-seam fastball having the highest. It is interesting to notes the two seperate velocity peaks within his changeup. Initially, it seems he either has two distinct flavors of a changeup, or that he isn't getting a good grip on them when he's throwing and the ball is coming out of his hand faster than he'd like. It will be interesting to see if his bimodal changeup is clustered into two different groups once analysis begins.

![alt text](https://raw.githubusercontent.com/jcusick13/baseball/master/pitch_classification/images/type.png "Pitch movement - as seen from the umpire's view")

When visualizing the dataset using just the horizonal and vertical movement of the baseball, we can see that some natural clusters begin to appear. Movement is measured in inches from the expected path of a theoretical pitch, devoid of any movement induced by spin. Therefore, this plot is not to say that all of his fastballs gain nearly a foot of elevation from where he released them, but rather that they have a higher trajectory than a similar pitch without any spin. This makes sense intuitively because fastballs are thrown with backspin, pushing the ball upward as it travels. Likewise, curveballs, thrown with forward spin, lose more elevation than a theoretical pitch of similar speed.