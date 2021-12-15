# Impossible Wordsearch

Impossible Wordsearch is a Shiny application for building wordsearches of custom dimension and content. It is capable of building wordsearches larger than the Guinness Book of World Records' largest wordsearch claimant of 150,000 characters, though in its current implementation it can only accept one word as an input.

# Instructions

Using the tool is as simple as running any Shiny application. The user may select the word they want to put in the wordsearch and the desired vertical and horizontal dimensions. Once the wordsearch is generated, the user is free to search for the word as long as they wish. If the user clicks on the wordsearch where they think the word is, the location will be revealed and they will know if they were correct.

# Ideas for iteration:
- Implement a scoring system with adaptive difficulties
- Implement ability to put multiple words in the wordsearch
- Implement ability to guess more than once for a penalty in score, each guess can remove a chunk of the board 
- Incorporate a "word does not appear" answer option for the hardest difficulties (would have some hidden % chance that the word isn't in the word search at all)