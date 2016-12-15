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

    The third optimization is adding a fitness function, which returns a fitnes value and assigns it to each state (domain [0, 1]).
    Fitness of a certain state depends on the distribution of the elements through the floors. The more elements are in the upper floors, the higher will
    be the fitness. We try to eliminate some of the states if their fitness is below a certain amount. This amount is the average fitness of states on a specific
    depth (concept borrowed from genetic programming, more fit states survive).
    There is a chance that we could drop a state that might have lower fitness, but would eventually lead us to the correct solution. This is why we
    use fitness sensitivity, to adjust how close should the fitness be to the average. In this case it was 0.023, which gave the correct answers to both parts
    of the puzzle in less than a second.
    
    Without this fitness optimization it takes about 10 - 15 seconds to get both part solutions (i7-4770K).
