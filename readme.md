# Skin and gesture detection

![MATLAB](https://img.shields.io/badge/MATLAB-R2021b-blue.svg)
[![GitHub Issues](https://img.shields.io/github/issues/mateuszschab/Skin-and-gesture-detection.svg)](https://github.com/mateuszschab/Skin-and-gesture-detection/issues)
![Contributions welcome](https://img.shields.io/badge/contributions-welcome-orange.svg)

## Basic Overview
The database used to perform the exercise:
http://sun.aei.polsl.pl/~mkawulok/gestures/index.html
Skin detection both by verifying the pixels against the HSV model:
+ H<= 0.1 or H >= 0.9
+ 0.2 <= S <= 0.6
+ V>= 0.4
+ Where H, S and V channels are normalized <0;1>.

Tested was an algorithm that saves mask values for pixels marking on a circle
with a chosen diameter r from the centroid. The number of sequences of ones gives the number of fingers visible in the 
image. One value must be subtracted from this number, which falls on the value of the hand. 
Diagram in the figure below.

## Visualize the results
![Skin_detected](https://github.com/mateuszschab/Skin-and-gesture-detection/blob/main/img_project/fig1.PNG)

The classification of finger visibility was based on different sets of images and the size of the circle based on the centroid. 

**Acknowledgements**
---

+ [West Pomeranian University of Technology in Szczecin](https://www.wi.zut.edu.pl/en/) Faculty of Computer Science and Information Technology - place of data collection and processing