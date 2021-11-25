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


## Milestone 2

### What is the architecture of your application (the key components)?

Our implementation have similar architecture as the starter code. The key componenets are as follow

1. Main  -- Where we start the game.
2. Control -- Where we allow the player to interact with the game, such as control the shooting angle and shoot.
3. View -- Where we design and render the corresponding components of the gameboard to the terminal GUI.
4. Model -- Where we define, initialize and update the game status.
1. Board -- Where we define, initialize and update the game board. The major effort goes to this component.
2. Player -- Where we define, initialize, and update the player angle, ball status



### What challenges (if any) did you have so far and how did you solve them?

One major challenge we met is that the documentation of Haskell is not as abundant as other famous languages, so it is sometimes hard to find the desired way to do a thing by searching things online. It significantly delays our progress, especially when we need to write a complete program from a language we are not completely familiar with. Fortunately, with extra effort and the help of hoogle, we were still able to create the function we wanted on our own.


Another challenge we met is that the shooting angle of a ball may not fit perfectly to a position where it should be most of the time. And it causes collision detection and bubble elimination to become very difficult. The solution we came up with for this problem is to estimate the closest possible location of where it can fit and only place the ball there. 


The third challenge we encounter is that the elimination of the balls with the same color may cause the other ball to detach from the wall and therefore are no longer on the board anymore. We do not come up with a very good algorithm besides repeatedly applying BFS on each node and checking if it is still attached to the wall. The problem with this algorithm is that it is not very efficient, and we may later need to improve the performance. The third challenge we enconter is that the elimination of the balls with same color may cause the other ball detached from the wall and therefore are no longer on the board anymore. We does not come up with a very good algorithm beside repeadtly apply BFS on each components and see if it is still attach on the wall. The problem for this algorithm is not very effiecient and we may later improve the algorithm. 





### Do you expect to meet your goals until the deadline?
We will be able to finish the implementation of a fully functional bubble shooter game. So it will meet our goals as expected. 



### If not, how will you modify your goals?
Although we anticipate we can finish the basic features of the game, we may not have time to add extra features such as replay, reload, falling ball animation, and scoring board by the deadline. Therefore we will focus on implementing the basic game first and see if we can improve it later. 




### GUI so far
![](https://i.imgur.com/QNBmXe7.png)
