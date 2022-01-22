# namegen

`stack run international_names.txt <n>` will train a simple model that learns
`P(nextCharacter|n-previous-characters)`, and spit out new names.

It tells when the names it generated were already in the training set (which
happens a bit, given the samples to generate and to learn on are pretty short)

As I suspected initially, n=3 seems to give the best results (about 70% of
generated names are truly new, and they are globally all making sense). Less
will result in gibberish, and more in dramatic overfitting.
