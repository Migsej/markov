$ Hello there!
$ How are you doing today?
$ I am fine, thank you.
$ That is good to hear.
$ Nice weather we're having today isn't it?
$ Yes, it's been very mild.
$ hello fine lady
# &fo "data/bbc_data.csv"
# &rs ∞
Classify ← (map◴:⊕□⊙:⊛.:)

# Model ? Data
Build ← (
  ⍜⇌(↘1) ↻2:◫2. # split data and find the following thing
  Classify      # set them up in map and classify them
)
Tolower ← ⍜▽(--@a@A)↧⊃(≥@A|≤@Z).

BuildWords ← (
  Tolower
  ⊜□¬∊ :"\n " . # split whitespace
  ∩(↙-1⧻.) ↻1.  # group up data with the following state
  Classify      # set them up in map and classify them
)
# Picks a random element
# Item ? List
Pick ← ⊡⌊×⚂⧻.
GetLast ← ⇌↙:⇌:
# Newout ? Out Model
Next ← ⊂⊙(Pick °□ get GetLast 2).

NextWord ← ⊂⊙(Pick °□ get ⊢⇌).

BuildWords
{"hello"}
⍢(⊸NextWord|has⊙,⊢⇌.)
# ⍥(⊸NextWord)100
# ⊸NextWord

# Build
# "He"
# ⍢(⊸Next|has⊙,GetLast 2.)
