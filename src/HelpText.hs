module HelpText where


helpText :: String
helpText = 
 "\n\n\
 \---- H-chu help text ----\n\n\n\
 \To make a play, specify the cards separated by spaces: eg.\n\
 \ '2 3 4 phoenix 6 7' will play a straight.\n\n\
 \Pass by saying 'pass'.\n\n\
 \To see your hand, type 'show'.\n\n\n\
 \Here are various other commands you can use:\n\n\
 \ 'tichu' -> call tichu (or grand tichu, if you've only seen 8 cards).\n\
 \ 'show tichus' -> see who has called tichu.\n\
 \ 'show cards' -> see how many cards the other players have.\n\
 \ 'quit' -> leave the game.\n\
 \ 'say <blah>' -> chat to the other players.\n\
 \ 'give left/right/partner <card>' -> prepare a card for the pass.\n"
