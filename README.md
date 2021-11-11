# Bubble Shooter 

The Bubble Shooter game is a famous game which is a clone of the Puzzle Bobble arcade. For a more creative touch, we will add additional game features such as the dropping celling and the bubble prediction.

The game will be implemented in Haskell with the standard bricks library. And we will use terminal line as our GUI. 

### Game Demonstration 

![](https://i.imgur.com/murdqDd.gif)


### Game Description 
The goal of the bubble shooter game is to clear all the bubbles on the ceiling. The player would have a shooter at the bottom of the screen. The shooter will randomly generate colored bubble based on the colors on the ceiling. When the player connect MORE than 3 bubbles of the same color, the connected bubbles would disappear.

In order to simply the game, we used the number to represent the color. Note that the bubble will bounce from the wall if the bubble hit the wall. In each stage, there would be multiple rows of bubble generated from the ceiling such as 3, 5, 7 rows. When the ceiling generate all bubbles, the ceiling will start to fall down by one row after each shoot.
After the ceiling fall to the ground, the game is over and the player fail. On the other hand, if the player clear all bubbles, the game is over and the player pass the game.


### How To Play
Players can move the direction of the shooter through left and right arrow keys. When players decide the direction, players can press the space key to shoot the bubble.

