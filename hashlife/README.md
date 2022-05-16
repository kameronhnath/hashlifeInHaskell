# hashlife

Hashlife in Haskell
CMSC488B Final Project by Kameron Hnath
5/16/22

Contents:
/app/Game.hs        contains the functions needed to run Conway's Game of life in the brick UI
/app/UI.hs          contains the functions that run the brick UI
/app/Main.hs        main function
/src/Hashlife.hs    contains the functions that make up the hashlife algorithm and its quickcheck property

Running the program:
Typing "stack run" will run the GUI for Conway's Game of Life. Make sure your window is expanded enough to fit the whole GUI. By default, the game will display Gosper's glider gun. This can be changed
to another sample configuration by changing the end of line 38 in the file "/app/Game.hs". Hit "q" to quit out of the game. Once you quit out, the quickcheck algorithm will run while displaying five
sample inputs, and then run 100 more times, displaying the results in the console. 

You can put my submission on the class website.