Haskenbush
==========

Game of hackenbush implemented in Haskell

Prerequisites
=============
* OS : Linux
* Haskell Platform


Haskell Libraries
=================
* hashmap-1.3.0.1
* gtk-0.12.5.7
* cairo-0.12.5.3
* glade-0.12.5.0
* random-1.0.1.1

How to run the game
===================
Using the make utility one can build game from source after installing all the required libraries. In case you are unable to compile because of conflicts with pre-existing libraries, you can run the application using the executable shipped along.

To run the game just type :
$ ./haskenbush

How to play the game
====================

You can play both Human vs Human as well as Human vs Computer with varying level of difficulties. At the start user can choose to play single or multiplayer. After this user is prompted to give the names of the players, followed by a screen where user is prompted to choose the difficulty level.

Once finished with the input user(s) can now play the game. Board consists of various bushes composed of several bushes colored using 3 colors Red, Blue and Green. Player 1 can cut Red/Green bush while Player 2 can cut Blue/Green bush. Computer (Jarvis) is the second player in a single player game.

Also, the board in randomly generated and thus keeping the player interested even after multiple plays. Apart from this the GUI is very user friendly where user can select the bush to cut by just clicking on the bush. In the status bar of the game user is informed which color bush he/she can cut and only those bush can be cut by the user(s).

At the end of game the status bar informs about the winner.

Contact
=======
Arihant Sethia +91-8011244745
Sparsh Kumar Sinha +91-7896381044