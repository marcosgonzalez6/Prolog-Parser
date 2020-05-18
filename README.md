# Prolog Parser with Wh-Movement

A syntactic parser implemented in SWI-Prolog that support _wh_-movement.


### Running the program
Go to https://swish.swi-prolog.org and choose "Program", an editor appears on the left. Copy and paste the code 
from the file parser.pl into the editor. Type `cp(CP,_,[Sentence],_).` in the text box on the right indicated by the symbol ?-.
Replace "Sentence" with a sentence made up of the words in the lexicon separated by commas without any end punctuation. 
For example: the sentence `Quiénvio a Irene?` would be entered as `cp(CP,_,[quién,vio,a,irene],_).` 
Click Run! on the bottom right corner to run the query. 

