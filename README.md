# Impossible Wordsearch

Impossible Wordsearch is a Shiny application for building wordsearches of custom dimension and content. It is capable of building wordsearches larger than the Guinness Book of World Records' largest wordsearch claimant of 150,000 characters, though in its current implementation it can only accept one word as an input.

# Examples
## Standard Display
![ex1](https://user-images.githubusercontent.com/11789070/146255358-5bd16dd3-8198-4d69-a676-48cf809b8f28.jpg)
![[ex1_solved]](https://user-images.githubusercontent.com/11789070/146255376-077a044e-2e2a-4509-a0d8-402702b2c2d2.jpg)
![ex2](https://user-images.githubusercontent.com/11789070/146255383-1b64695b-73b9-45a9-8183-013b15a7b7e1.jpg)
![ex2_solved](https://user-images.githubusercontent.com/11789070/146255393-6ba607d0-8c73-4322-9c88-3a74af2bcd70.jpg)



# Instructions

Using the tool is as simple as running any Shiny application. The user may select the word they want to put in the wordsearch and the desired vertical and horizontal dimensions. Once the wordsearch is generated, the user is free to search for the word as long as they wish. If the user clicks on the wordsearch where they think the word is, the location will be revealed and they will know if they were correct.

# Ideas for iteration:
- Implement a scoring system with adaptive difficulties
- Implement ability to put multiple words in the wordsearch
- Implement ability to guess more than once for a penalty in score, each guess can remove a chunk of the board 
- Incorporate a "word does not appear" answer option for the hardest difficulties (would have some hidden % chance that the word isn't in the word search at all)
