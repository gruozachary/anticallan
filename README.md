# anticallan
My friend kept beating me in the IMessage game *Word Hunt* - I wanted to change that.

This is a small program to find all possible words in that game. To use it, build the program using stack and then run with two command line arguments:
```
./out <characters> <word list system path>
```
Where the word list file is plaintext with each new word denoted by a new line.

e.g.
```
./out RSPEHYPNAUTOYERO ~/Desktop/wordlist.txt
```

This will output a list of all the words in the grid, and instructions on how to trace them out using your finger using the cardinal directions.

This program is currently only compatible with the default gamemode.
