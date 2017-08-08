# ICFP 2017 Submission

This is my solution to the ICFP 2017 "Punting" challenge. It didn't go
fantastically well in terms of time, but I had fun and learned plenty.
I've published it here for reference, but don't expect it to be anything
other than a bundle of technical debt.

This year I solved this alone again. I should probably have been more
proactive finding team mates but planned everything fairly late and
never got around to sending out any queries.

## Premise

The problem this year was to write an AI to
play a game themed around claiming exclusive use of rivers (canals?) to
transport the product of ICFP 2012's now successful Lambda mines to the
desperate consumers by punt. This AI must be sent to the judges, where it
will compete against other people's solutions.

Based on the concept of enclosing canals for private use, I gave my team
the dual meaning / pun name "Lambda In Closure Act".

## Early Strategisation

The game appeared to resemble Go (https://en.wikipedia.org/wiki/Go_(game))
in a number of ways. While this was played on an arbitrary graph, and by
claiming edges rather than the points (vertices) of the graph, there were
some strong resemblances.

AI-wise, the branching factor of this game is way too high to employ
chess-style strategies where we predict moves in advance. Even predicting a
single round ahead would be very computationally intensive, and we were only
given one second to make a move.

I chose to base my strategies around valuing edges, and finding choke points
in the graph. At any step, the planner would proceed through as many strategies
as it had time for, with each strategy self-scoring so that it can keep the
top scored move and play it when the time gets low. I set the time-out to half
a second to be conservative.

## Language Choice

I chose to write this year in Haskell, which I've used for a previous ICFP
but otherwise have little experience with. My considered choices were Ruby,
C, Haskell, or Rust. I'm most familiar with Ruby, but ruled it out this time
on the basis of having hard performance limits, so I wanted something compiled.

I've used C before for the Lambda-the-Gathering challenge and it worked well,
but my early thoughts on strategy said I was going to do a lot of graph
manipulation. I needed efficient graph structures and was going to do a lot
of copying and disposing of them, so didn't want to spend time on memory
management.

I briefly considered Rust a couple of weeks back, then I met its borrow
checker and decided 72 hours wasn't enough to get the knack of that. It might
be safer than C, but it doesn't seem a lot easier than C.

I ended up choosing Haskell as it tends to work well for these types of
challenges, and seemed to have good Graph libraries available for the type of
AI I wanted to build.

## Implementation Issues

This decision to use a less familiar language required me to do a lot of
reading during the challenge. I had about a dozen tabs open at many times just
with the Haskell standard libraries, trying to work out that a list it checked
for emptiness with the "null" method, not the "empty" method. JSON parsing was
new to me so all the IO took quite a while.

As a result of being on my own, and the amount of work involved, it took me
two days to get the program applying updates and reliably playing in the
online and offline modes before I could start implementing strategy.

## Extensions

Through the challenge, three extensions were added to the game. These were
not guaranteed to be available in all games, and I never ended up implementing
any of them within the time frame.

### Futures

Futures let you place a bet that you could connect each mine to one given
target. This would give you a substantial reward if you succeeded, and deduct
the same amount if you failed. I concluded that predicting these accurately
was beyond my immediate means, so ignored the extension.

### Splurges

Splurges let you pass for a number of turns, then place a connected river all
at once. I considered using this, but decided in the end that it was a
complication and didn't add a large margin. Therefore I implemented the minimum
to cope with others playing this move, but didn't use it myself.

### Options

Now this was one I wish I had time to use. It allowed you to gain use of
any claimed river, with a strict limit on how many times you could do so.
This actually made the last two options worthwhile - futures would be easier
to achieve and splurges more likely to succeed. Unfortunately, I never had
time to implement this one, but that will be a major loss point for my
program.

## Graph-Theoretical Planning

The very first strategy I wrote was random selection. This remains the default
when the other strategies don't kick in.

I focussed on a reduction approach for my actual strategies, assuming that I
could currently use all unclaimed rivers and valuing what losing any given
river would cost.

The first actual strategy I implemented was to detect when parts of the graph
are about to get cut off - when they're connected only by a single available
river (called a "bridge" in graph theory). Unfortunately, despite the otherwise
fantastic algorithms available in Haskell's fgl library, whole-graph bridge
finding seemed to be missing. I therefore had to implement this to use this
approach. The simplest linear bridge-finding algorithm I could find was
Jens Schmidt's chain decomposition technique [1].

Each bridge is valued as if I were about to lose that link, based on the points
gained by the mines on each side connecting to the consuming sites on the other.
As I'll note later, there was however a critical bug here.

This took about half a day
to get working and works ok in small games with few opponents, but wouldn't
work at scale as you'd only see a small % of bridges before someone else took them.
This required a more general concept of finding bottlenecks. This is
too expensive to do universally, so the planner performs multiple rounds of
selecting two random mines and finding the smallest number of unclaimed
edges that could be removed to cut the two off from each other.

This uses
the maximum flow algorithm in FGL to find which rivers are saturated and
therefore where the minimum cut occurs. It focuses on connecting mines as
this seemed to be the key to high scores.

I ran out of time here, without implementing any strategies for using the
extensions added to the game. My next choices would have been to re-connect
disconnected areas using any available options.

## The Bug

About two hours before the deadline I stopped all feature development and
started packaging and debugging.

One minute before the deadline I found a critical bug in my code. When valuing
the split between two parts of the graph I did this by copying the map, removing
that edge and working out the loss caused. Unfortunately, in making the rivers
navigable in both directions I had added links in both directions, and only
removed them in one.

This will have led to my planner's valuations being well off, and will lead
it down the wrong track.

## Other Changes I'd Make

Apart from fixing that bug, I'd like to continue optimising this a bit more
over time. I'll need to find a server (unless the organisers release theirs?)
to do this.

## References

[1] Schmidt, Jens M. (2013), "A Simple Test on 2-Vertex- and 2-Edge-Connectivity"
    Information Processing Letters, 113 (7): 241–244, doi:10.1016/j.ipl.2013.01.016.
