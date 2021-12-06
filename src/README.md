# Day 1 Part 2

So, we have 10 lines in the example which is exactly 1 cycle
for the puzzle.  The puzzle data is the same file that
we processed in Day 1 Part 1, which has 2000 entries.

Hence, there will be 200 cycles.

There are 8 sum to be processed per cycle, 1 sum per letter.

It would probably be easiest to get this correct by assembling
each of the data pairs as follows:

| Prev  | Next  |
|-------|-------|
| x     | x + 1 |
| x + 1 | x + 2 |
| x + 2 | x + 3 |

There will be 1600 of these calculation sets.  The answer is
**1** if the sum of the numbers in the `prev` column is less
than the sum of the numbers in the `next` column; else **0**.
Adding the 1600 sets should yield you a number in the range `0 .. 1600`.
