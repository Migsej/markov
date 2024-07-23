$ Hello there!
$ How are you doing today?
$ I am fine, thank you.
$ That is good to hear.
$ Nice weather we're having today isn't it?
$ Yes, it's been very mild.
# Model ? Data
Build ← (
  ⍜⇌(↘1) ↻2:◫2. # split data and find the following thing
  map◴:⊕□⊙:⊛.:  # set them up in map and classify them
)
# Picks a random element
# Item ? List
Pick ← ⊡⌊×⚂⧻.
GetLast ← ⇌↙:⇌:
# Newout ? Out Model
Next ← ⊂⊙(Pick °□ get GetLast 2).

Build
"He"
⍢(⊸Next|has⊙,GetLast 2.)
