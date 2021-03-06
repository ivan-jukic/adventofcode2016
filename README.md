# Advent of Code 2016
### [http://adventofcode.com/2016](http://adventofcode.com/2016)

In this repo I'll be putting my solutions to daily puzzles of 2016 adventofcode challenge.

I am using [Elm lang](http://elm-lang.org/) for my solutions, a decision I've made with hope that these challenges will improve
my understanding of the Elm language, and maybe even help some of you guys who are reading and are interested in this.

---

- ### Day 11 - Update
    [http://adventofcode.com/2016/day/11](http://adventofcode.com/2016/day/11)

    This was a challenging puzzle. Although it was clear how to get to the solution from the start, there were some difficulties along the way.
    I'm not sure if Elm is the right tool for this kind of problems, or it was my lack of experience with it, but I had to rewrite the code in
    something that was more familiar, so I've used C#.

    I tried to optimize the code as much as possible to get the result as quickly as possible. For brute force approach, the number of possible
    states is huge, and it would take some time to go through them all, so to avoid this, the first optimization was to prune already visited states.
    Also, a hint I've received, ignore the type of the elements when comparing states, and consider them the same as long as the element pairs are on 
    the same floors.

    The second optimization blocks return to the first floor, if in some previous state first and second floor were empty. This is a small optimization
    that takes effect only when we're already close to the solution, but it seemed like a reasonable thing to test. It is also in conflict with
    the next optimization, and may not be neccessary at all.

    The third optimization is adding a fitness function, which returns a fitness value and assigns it to each state (range [0.0, 1.0]).
    Fitness of a certain state depends on the distribution of the elements through the floors. The more elements are in the upper floors, the higher will
    be the fitness. We try to eliminate some of the states if their fitness is below a certain amount. This amount is the average fitness of states on a specific
    depth (concept borrowed from genetic programming, more fit states survive).
    There is a chance that we could drop a state that might have lower fitness, but would eventually lead us to the correct solution. This is why we
    use fitness sensitivity, to adjust how close should the fitness be to the average. In this case it was 0.023, which gave the correct answers to both parts
    of the puzzle in less than a second.
    
    Without this fitness optimization it takes about 10 - 15 seconds to get both part solutions (i7-4770K).


- ### Day 14 - Update
    [http://adventofcode.com/2016/day/14](http://adventofcode.com/2016/day/14)

    This challenge wasn't as difficult to implement, as it was to realize what is actually required. I feel like it could have been explained better.
    The description can be interpreted in multiple ways, so it is on developer to impliclity figure out what is required. I wonder if this was by design(?).

    In part 1, the description explicitly says that hash is a key only if it contains three of the same character in a row, and one of the next 1000 hashes
    in the stream contains that same character five times in a row. But implicitly you need to take into consideration as possible pad any hash with three or more
    same characters in sequence. Also hash with five or more same characters in row used to validate previously found possible hashes, is itself a possible pad.

    Part 2, also holds a catch. It asks you to hash input 2016, but you need to hash first hash 2016 times, or raw input a total of 2017 times.

- ### Day 19 Part 2 - Update
    [http://adventofcode.com/2016/day/19](http://adventofcode.com/2016/day/19)

    Tried to implement a simulation in elm, but unfortunately elm doesn't agree with big lists. In the end I took mathematical approach which does not
    require any implementation. Part 1 does do a simulation, but it involves string manipulation, which doesn't make it a really clean solution.