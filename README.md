# How to survive ETH Algolab as an idiot (Fall 2024)
In this guide I will write down everything I learned about passing the course Algolab at ETH. I took the course in the Fall semester 2024. 
If you have a competitive Programming background or are really really smart, then this course should be no problem for you. In case you are not (like me) then this guide is for you.

**Structure**:
1. About the course
2. how to survive the semester
3. how to survive the exam
4. Solutions

## About the course and why one would take it
In the CS ETH Master you have to pass 2 labs out of the 4 options. Algolab is known to be very difficult, frustrating and having a hard exam (instead of being project based like other labs). So why the hell would you take it? Well in case you aren't very interested in Information Security but rather in algorithms like me then maybe this course can make sense for you too. I didn't have Competitive Programming background, which makes this course very difficult and a lot of work, but my idea behind taking this course even though everyone says it's horribble (it truly is) was: If I am able to pass this course (I don't care about my grade in algolab) and put myself trough all the pain and suffering it will improve my logical and algorithmic thinking and programming skills. And it did! Was it worth it? I don't really know. Would I take it again if I could go back in time? Probably yes.

## How to survive the semester
I would recommend solving at least 85% of the problems. You really need the very frustrating practice and pain to learn how the problems are structured and work and to learn BGl and CGAL. They are super annyoing and take time to get used to, but you 100% need them to solve the exam questions. I would recommend to always work with the official DOCs page you will also have access to during the exam. That way you can get used to the material and searching up stuff just like in the exam. On there are also the example-code snippets, those are absolutely essential! Whenever you work with anything other than DP you will need them. 
Don't spend too much time solving one problem, but also don't look at the solutions after 30 mins of trying. I would give it between 1-3 hours and then look up the solution if you are stuck; but make sure to then fully understand what you did wrong and how to properly solve it. Under no curcimstance just copy and paste the solution code!!! Try to understand the solution and then code it up yourself, otherwise you will only learn a fraction as much.
Don't get too demotivated by the problems. They are really difficult, I don't care what anybody else says.
Week 10-14 is the exam preperation time. When week 10 hit the difficult felt like it increased by a lot. Don't let this intimidate you, just keep going and when you are done with almost everything you will have a much better feel for all the problems trust me.

## How to survive the exam
There are 2 exam days with 3 Problems each. Each problem can give up to a 100 points. So 600 in total. Each exam day one of the three problems is a known one from the semester but with a different backstory. Those are the lifesavers! Don't miss out on those! I have friends who couldn't fully solve them or took 2 or more hours to do so. This is bad bad bad. It is sooooo much harder to solve new problems than learn the known ones from the semester. I would recommend everyone to learn all the problems from the semester. Don't just have a few or even more problems that you never even read or know the solution, why would you?! I made a list the week before the exam where I wrote down how to solve every single problem and went trough it all the time and I got both known problems 100%. For me there are three levels to knowing the problems: 1. Knowing the conceptual solution 2. knowing the code 3. Knowing how to code it yourself. For me there is a difference between knowing the conceptual solution and actually going trough the solution code itself and seeing the constructs and the exact code on how to solve it. I personally had a few problems that I never coded up myself, but I learned the conceptual and code solution for them. And in fact the known problem on the first day of the exam was one of those problems and I was bale to code the right solution in 40 mins because I went trough the actual solution code of someone else and tried to understand it. Going trough someone elses code is super annyoing but in my opinion if you can go trough it and actually understand properly in detial how it works you will be able to also code it yourself.

Here are our exam problem-topics.
1. Exam Day
   - DP Problem (very hard imo, only solved 50%, but those were very doable; apparently you needed to use some sort of dp-sweeping algorithm)
   - Graph problem (Djikstra) (You had a grid with weights and needed to compute the path from a starting to finish point with the smallest amount of damage; just buold the graph and since the weights should be on the nodes and not the edges you just used the duplication trick, so every node in the graph gets split into an input and output node and you put the weight between the edge ebtween them. Test cases 3 and 4 were about being able to skip an amount of weights c, but only up to like 10 or 15, so you could duplicate the graph that many times and connect the in node to the same outnode of the duplicated graph.)
   - Hermione granger clone (Delaunay + Linear program)
  
2. Exam Day
   - Some graph + DP Problem I think (not a single clue on how to solve it, only got the first test case)
   - Mincut Problem
   - The Hands Tourney Clone (Delaunay + Union Find)
  
What I learned from them:
Both of our clones were from week 10, as far as I know the two clones from 2023 were from week 10 and 11. So I have this theory that the clones might be usually from the exam preperation phase (Week 10-14) rather than the weeks before. Maybe if we gather more data trough the next years we can be more sure of it. Also as much as I kind of hate algolab I have to admit: Our exam wasn't that mean, because you could literally pick up half of the points for a problem without a super fancy crazy solution. So if you get both known problems (200 points) and then just greedily get the test cases you should be able to pass! This another recommendation of mine for idiots like me: Greedily get the test cases! Don't be too focused on getting the right overall solution. Most times the test cases are structured in a way that you build up the solution along the way, so if you just build it step by step you can get points all the time. Some friends didn't have the time in the end to solve the easy test cases anymore because they focused on solvin the whole problem at once (kinda). If you are really good at this stuff and want a really good grade, then you can try to solve the problems fully directly; but if algolab is as annyoing and difficult for you as it was for me then I would recommend my approach. Also as a sidenote: Sometimes the solution for the first or second test case are entirely different to solving the whole problem and you could spend a lot of time implementing a solution there instead of solving the whole problem; for this you just got to get a feeling for the problems and test cases and get lucky. I'd rather have 50 points in the bag and less time for the rest of the problem left; also often times you learn more abou how to solve the problem if you do it step by step instead of in one go.

## Solutions

Here is a incomplete list of written conceptual solutions to the problems I used to preapre for the known 2 problems in the exam.


**Coin Tossing Tournament - Maxflow**
“There is a schedule of rounds. In each round two players face each other. The number of
rounds and the players who compete in each round are predetermined before the proper game
starts. Then, the outcome of each round is decided by a simple coin toss – the winner gets one
point, the loser zero. In the end the points of each player are added up and the scoreboard
with each player’s result is assembled. You wrote down the schedule of rounds, but in the ensuing excitement you did not manage to record all the results of the coin tosses – only some of them. Now, your friends show you a scoreboard that clearly indicates you lost! Good-hearted as you are, you would like to check if it is even possible to obtain this scoreboard given the schedule of matches and the results you had noted. After all it should take some non-trivial effort to play a prank on you. . .”
Sum up the score for each player with the known games (c!=0)
Add a vertex for every player and if the player’s score is the same as on the scoreboard do nothing; if it is bigger it obviously can’t be and if it is smaller we add the needed amount from the player vertex to the sink. Then for every unknown game we add a vertex connected to source with capacity 1 and to the two players with capacity 1 each as well; so only one player can sort of get the point there and win. Then we just push the maxflow and see if the flow arriving at the sink is equal to the missing points or not.

**Knights - Maxflow**
Have a weird maze setup with some amount of knights stationed in there that want to escape. Every pathway can only be used once and intersections can be used a maximum of C times. Question is how many knights can safely escape?
Model it as a graph, but since every pathway can be used only once we split it up into two nodes, so we have an input node connected to the output node with capacity 1. We model the intersections in the same way but with capacity C. The main issue really is the connecting logic, since we need special cases for all border and the normal cases for the north south and west east connections, but basically we create an in and output node like we described. 

**Kingdom defence - Maxflow**
“You are given a map of the (directed) paths between the most important strategic
locations in the kingdom of swamps. Every location has a number of soldiers available and
every location needs a certain number of soldiers to defend itself. In order to display military
presence, each path has to be travelled by soldiers a certain minimum number of times. Also,
because soldiers on the move constantly need to supply themselves with food, each path can be
traversed by soldiers at most a certain maximum number of times. Note that a single soldier
may use the same path multiple times and each time counts as one traversal.
You have to decide whether it is possible to satisfy all these requirements by moving battalions
of soldiers around in the kingdom.”
Compute demand for every location as = soldiers available - soldiers needed in the end.
Then for every directed edge we have a minimum number of traversals and a maximum number; we can ensure this minimum traversal constraint by simulating the amount of soldiers already having gone that path by subtracting that minimum amount from the demand of the source of the edge and adding it to the target demand of the edge.
Now we model every location as a node which get’s the demand from the source if positive or goes to the sink if negative. Then we add all edges with capacity: Maximum Traversals - Minimum Traversals.
If total demand (so all negative demand combined) is equal to the Flow it’s doable otherwise not.

**Octopussy - Greedy**
Pyramid of bombs each with a timer when they will explode. It takes bond one minute to deactivate on bomb, but they are stacked weirdly on top of each other so you first have to deactivate all the bombs on top/bottom of it.

Idea: Greedily sort the bombs according to their explosion time and try do deactivate the one with the smallest time first, since obviously if we can’t take deactivate the one with the shortest timer we are cooked anyways. 
Go through the sorted list and if not yet deactivated try to deactivate that bomb by calling a recursive function to deactivate all that stand on top of the bomb we are trying to defuse. Keep a bool vector of the ones already defused and a timer. Add the time it takes to defuse the required bomb to the timer and if it is bigger than explosion time we’re done.
“
int take_subtree(std::vector<bool>& taken, const int root) {
	if (root >= (int)taken.size() || taken[root])
    	return 0;

	taken[root] = true;
	return 1 + take_subtree(taken, root * 2 + 1) + take_subtree(taken, root * 2 + 2);
}
“

**H1N1**
“Your program has coordinates of all known infected
individuals at its disposal. It is supposed to answer queries whether a given user of the device is
able to escape the contaminated area, assuming that all infected individuals stay at their current
location. In order to escape, the user must be able to get arbitrarily far from all infected people,
without ever coming closer than a given distance d to any of them”
Use Delaunay Triangulation and then walk along the voronoi edges to see if you can escape eventually. => Not fast enough => Precompute the maximum size one can escape from a given voronoi node.
Add a pair of (bool, K:FT) to the vertex info in the triangulation thing, were bool indicates if you’ve processed this node already and K:FT the maximum size that can escape there.
First go trough all faces and check if one of the neighboring faces is infinite, if yes add the squared distance between them to a priority queue with {edge-length, face}.
Then go trough the priority queue in a loop always taking the one with the biggest edge-length and then go trough all 3 neighboring faces and compute the maximum escape capacity as:
“auto capacity = std::min(t.segment(curr.second, i).squared_length(), curr.second->info().second);”
Then we check that the face is not infinite and hasn’t been processed yet and then add the maximum capacity into the node info and add this combination to the queue. After processing the neighbors we set our current node to true being processed.

**Germs - Delaunay**
N germs with 2D positions and a starting size. They grow at a rate: t^2 + ½ and when two bacteria touch each other they both die but keep growing; or if they touch the border they als die. For every test case the corresponding output appears on a single line consisting of three non-negative integers f m l, where
• f denotes the moment in which the first bacterium dies,
• m denotes the moment in which the amount of bacteria still alive goes below 50%,
• l denotes the moment in which the last bacterium dies.
Precompute the closest obstacle for each bacteria through Delaunay Triangulation and then sort according to the distance. Now just compute the time of death backwards for the first the middle and the last distance in the vector:
“  K::FT d = CGAL::sqrt(square_dist);
  return (int) std::ceil(CGAL::sqrt((d - 1) / 2));”
Caution: You have to subtract 1 and divide by 2 for the distance between two bacteria since they both grow, like in the code snippet above, but then you have to be cautious for the collision with the wall:
“
	long x = vh->point().x();
	long y = vh->point().y();
	long r_dist = std::min(r - x, std::min(x - l, std::min(t - y, y - b)));
	r_dist = 4 * r_dist * r_dist;
“

**Clues - Delaunay, Bipartite, Graphs**
Given n towers coordinates each with a radius r, then we get 2 coordinates and want to know if they can talk/connect to each other. But they have their own frequency but the towers have 2 available but no two towers of the same frequency are allowed to be in range of each other.
First part is to make sure the towers can be two colored; problem there is if we use delaunay we only get the closest edge right and not all towers in radius r; so we first do Delaunay triangulation and if the closest tower is farther than r away it doesn’t matter anyways; but if it’s closer we go through all delaunay edges for every node and then do a bfs for every connected node that has a distance smaller than r to the original node we are doing. That way we we build a new graph where we then check the two color property by just calling “  bool interferences = !boost::is_bipartite(g);” if it is not bipartite we can print out no for every request.
Otherwise we do delaunay on the new graph again and for every pair of coordinates we get we first check if they are in range of each other anyways, if not we do a delaunay call and look if the closest tower is in reach of r and if they are we do a connected components call from the two towers to see if they are reachable from each other.

**Attack on Kings Landing - Dijkstra + Maximum Matching**
N cities and each city can have a fire brigade than can only build barricades within distance d. We want to maximize the amount of saved roads/edges and we can only repair them by building a barricade on both sides of the edge. For normal cities only one barricade can be built and for plazas 2 but now two plazas are connected. First do Djikstra for the distance limit  by adding a source node that is connected with 0 cost to all cities with a brigade since the distance that they can travel is all the same and then maximum matching where you just split up every plaza into two nodes; this works since when one node connects to a city the other one can build another one but not the same one since the city the first one connected to can only build one barricade.

**Placing Knights - MaxIS**
Let us have a square chessboard of a given size with holes. How many knights can you place
on the chessboard so that no two knights threaten each other. Observation: Black knights only threaten white squared and vice versa. Sadly, we cannot use the flow information to count the possible number of knights to place on the board. This is essentially a bipartite problem. We connect every black square to the source and every white one to the sink. Now we add all edges from each black square to all white squares it attacks. Now we push maxflow and then take the Mincut/MaxIS code to calculate the Maximum Independent Set; this logically is the biggest set of vertices (knights) that aren’t connected to each other at all, since the edges in the graph mean they could threaten each other this naturally means the maximum number of knights that don’t threaten each other.

**Real Estate Market - MincostMaxflow**
Sell M pieces of land and N potential Bidders with M bids each. Maximize Profits. Constraint: States S have a limit how many pieces of land can be sold there and every piece of Land is linked to one State. Just model every piece of land as a node and connect it to the according state and every state with capacity l to the sink. Now for every bidder connect each bid to the according piece of land.

**Canteen - MincostMaxflow**
Min cost max flow where each day is a vertex. Then, we can keep meals between days by vertex between consecutive days. Add meals each day and sell meals each day.

**Algocoon - Maxflow (Mincut)**
You have n nodes and m connections/edges between them. You want to find the cheapest option to split them up. We are looking for a price of the cheapest cut - note that the problem does not ask for the cut itself. If we knew the division of the two sets (one is figures for me, second for my friend) of the figures then we could pick any pair of figures - each from one set - and the maximum flow between them would be the minimal achievable cutting cost. Note that the maximum flow is not symmetric - the maximum flow between figures A and B does not necessarily equals the one between B and A. Therefore we might assume that both should be checked. Since we know both sets are non-empty we can fix one figure and check the max-flows to all other figures. Besides that we also need to check the max-flow in the other directions. We are guaranteed that at least one figure will be in the other set. This requires 2*(n-1) calls to the max-flow function.
“
long flow = std::numeric_limits<long>::max();
  for(int i = 1; i < figure_count; i++) {
	flow = std::min(flow, boost::push_relabel_max_flow(G, 0, i));
	flow = std::min(flow, boost::push_relabel_max_flow(G, i, 0));
  }
“

**Hermione Granger - Delaunay + LP**
Just do Delaunay and set up LP. Be aware: Two members can be closest to the same student then we need to greedily decide who to pick there.

**Asterix and the Tour of Gaul - MincostMaxflow**
N stops/nodes and every node has a limit ci of how much food we can bring trough there. WE basically have a list of food items where each item needs to be picked up at stop a and brought to stop b and has a significance. We want to maximize the overall significance while keeping the constraints.
=> Model it as a graph (duh) and first for every combination of trips from a point a to b we calculate the smallest limit of the cities/nodes in between and then for all items that go from a to b we only take the ones which bring the most significance and maximum so many as the min capacity in between there.
Then for every item we create an edge from city a to city b with capacity 1 and cost of the significance.
Inbetween city i and i+1 we add the edge with capacity from city i. This should make sense.
Now for every transition from city i to city i+1 we compare the capacity constraint. If it becomes more we add that extra from the source to that city and if it gets less we subtract that amount by adding the amount as an edge to the sink.
Basically how this works is we don’t need the actual maxflow flow number, we only want the cost in the end. Basically at a city where we get extra flow that flow can be used to take an edge from that city to it’s destination city and then we sort of delivered that item and got the profit from the edge. Now the flow there can be used again to transport an item from there to somewhere else. Or maybe the capacity there got smaller so this flows to the sink.

**Rubeus Hagrid - Graphs (Greedily sorted DFS)**
Binary Tree with every edge having a length. Some nodes have an amount of gold, but the time x it takes to get there gets subtracted from the amount of gold there. The question is what is the maximum amount of gold you can retrieve from the starting node at the top.
First we recursively precompute for every node the amount of children that node has and the total traveltime for the whole subtree. Then we recursively calculate which subtree to take first, so we first sort all subtrees/children according to the time it takes to traverse that subtree times the amount of chambers with gold we have there: “(w1 + weights[tgt1]) * counts[tgt2] < (w2 + weights[tgt2]) * counts[tgt1]” .
Then:

“
long sum = galleons[src] - time;
  for (const auto [tgt, weight]: children) {
	sum += solve(g, tgt, time + weight);
	time += 2 * (weight + weights[tgt]);
  }

  return sum;
“

Then we explore the subtrees in the order of how we sorted them. We recursively calculate the sum by calling that subtree with the current time + the time it takes to travel to that subtree. Afterwards we adjust the current time by taking the time to get there + the time it takes for the whole subtree times 2 (for going down and up again) and add that to our time.

**San Francisco - DP**
Directed Graph and every edge has a score. There is a starting node and nodes with no outgoing edge teleport you back to the starting node with no step being used. Given score x in k moves find either the minimum amount of moves to beat the score k or prove that is is impossible to score x in k moves.
=> Solve by using a 2D DP table. dp[a][b] is the maximum score we can get by arriving at node a in b moves. Then we iterate over the amount of moves from 0 to k and in that loop over every node that has a dp values >= 0 (we initialize with -1 and dp[0][0]=0). Then for dp[node a][moves] we look at all outgoing edges and see if our current dp value + the score of the edge is bigger than dp[arriving node][moves+1] and if so we update it. This way we incrementally build the dp by always updating the value of the dp table for moves+1. For the special case with the nodes with n outgoing edges we check dp[0][moves] < dp[u][moves] and update it in that case and then we do a normal update like we do with all other does but just starting from node 0. Then in the end we check every nodes dp table values for the minimum steps to get a score > x and then take the minimum over all those values.

**The Hands Tourney - Delaunay + UF**
We have n points (tents) and want to assign each tent to a family. Each family needs up to k (1-4) tents each, but tents of different families need to be at least s squared distance apart. First we are given a number N of families and are supposed to return the minimum distance s so that we can still accommodate N families. We can take edges from the Delaunay triangulation, sort them, and start merging the clusters starting from the smallest edge. In each step we check how many families we can still accommodate. Once we drop below N we take the squared length of the edge. This is due to the definition of the problem - non-strict inequality. In the second query we are given number D and are tasked to provide the maximum number of families to accommodate such that tents of different families are at least D (squared) apart. We can just iterate over the edges and and merge all clusters that are less than D (squared) apart. Then we just calculate the number of families from the cluster counts.

**Cerenyan Hind - Maxflow**
We are given a graph with a starting point and every node has a convenience score ci. We are trying to find a semi dead end with the biggest combined convenience score. Formally, a semi-dead end is a non-empty subset S of vertices such that no edge leaves S. But S can conceptually contain multiple parts that are semi dead ends of the graph so essentially we have to sum up all the positive Semi dead ends S that are different from each other (if that makes sense at all). So we simply connect the graph with every edge having a very high capacity and then for every positive convenience score we get that amount of flow from the source to that node and for every negative score we connect that node to the sink. Then the answer is the total sum of all positive convenience scores - flow. Essentially when the flow goes trough a path it gains flow from positive nodes and loses flow from the negative ones. So if we have for example 5 positive flow going into a node with negative score 8 we lose all of our flow there so it is not part of a subset S there, but in case we only lost 3 then we have sort of 2 positive flow that keeps going.

**Asterix in Switzerland - Mincut**
There are n provinces in the Roman Empire. Each province pi has a central bank with a
balance bi that can be either positive (surplus) or negative (deficit). Moreover, some of the
provinces are in debt to others. That is, di,j is the amount of money that province pi owes
to province pj. One or several provinces want to form a new union, so that the debts among
them can be canceled. A union of provinces is called free-standing if their total balance minus
the total debt to provinces outside of the union is positive.
=> Just give connect all positive b’s to the source and all negative ones to the sink and then push the Maxflow and see if it is smaller than the total positive

**Learnean Hydra - MincostMaxflow**
Hydra has n heads and a head i can only be permanently slain if all heads j < i have been slain already. There are m eradication patterns (that can overlap and be combined). What is the minimum cuts needed? We can simply sort all patterns based on the last head in the pattern, the one actually slain, and then connect source to all patterns that slay head 0 and then for every pattern that slays head i we connect it to every pattern that slays i+1 and the cost is equal to the size of the pattern in i+1 - the overlap.

**Sith - Binary Search + CGAL Delaunay**
This problem might look complicated at first but it falls into the category of problems that can be easily solved by binary search and verification rather than crafting the optimal solution. The shown solution includes the following steps/techniques:
Binary search
Delaunay triangulation for graph construction
BFS (or DFS) to find connected component on a graph
Avoiding SQRT by comparing squares
Binary search
The main idea in this problem is to use binary search on the value K. In each step we omit the first K planets/vertices from the solution and look for a connected component in the graph or more specifically C - the size of the biggest connected component. We can make few observations:
With increasing K the resulting graph is getting smaller and hence C is decreasing (not necessarily in every step)
Given K and calculating C the result for this setting is min(K,C). We are therefore balancing these 2 values.
The initial upper bound (the "high" index) can be set to N/2. C cannot be bigger than N-K. This might not seem important as this would save 1 step in the binary search, but but in each verification step we are given graphs with different size, therefore we want to avoid checking for big values. Theoretically this can speed up the whole computation 2x.

“
  int best_result = 0;
  int low = 2, high = nr_planets/2;
  while (low <= high) {
	const int mid = (low+high)/2;
	const int max_component_size = max_connected_component_size(planets, mid, squared_range);

	if (mid == max_component_size) {
  	best_result = mid;
  	break;
	}
	if (mid < max_component_size) {
  	best_result = std::max(best_result, mid);
  	low = mid+1;
  	high = std::min(high, max_component_size);
	} else {
  	best_result = std::max(best_result, max_component_size);
  	low = std::max(low, max_component_size);
  	high = mid-1;
	}
  }
“

The only thing to do is for given K and calculated C specify the update rules for "high" and "low". We have only 3 options:
K == C - We found the result/optimal value. Decreasing K would decrease the result (since it is min(C,K)) and increasing K is also of no avail as it could only decrease C.
K < C - We can increase K to potentially better result. We can also cap upper bound (for K) to C since making it bigger would give no benefit.
K > C - We can decrease K to potentially better result. We also know that K cannot be smaller than C since decreasing K to C would make C to be same or grow, therefore going with K even lower would lead to no gain.
Delaunay triangulation for graph construction
In this problem we are not given the graph explicitly (edges are not specified), instead we are given only the vertices (their 2D positions) and distance R such that two vertices are connected if their distance is not bigger than R. One approach is to iterate over all pairs of vertices to find all edges, however, this is not necessary since we do not all edges - we are looking for connected components. We can use/construct Delaunay triangulation which is guaranteed to contain the EMST (Euclidean minimum spanning tree) check all potential edges and ignore those with length > R.
BFS (or DFS) to find connected component on a graph
Having the edges of the graph we can find the biggest connected component by running DFS/BFS and marking visited vertices. Since we are operating with CGAL Triangulation we do not access vertices by integer index but access them via an iterator and vertex_handle. Instead of making bool array we can attach the bool to the vertices using CGAL::Triangulation_vertex_base_with_info_2<bool, K>.
Avoiding SQRT by comparing squares
When checking if potential edges from the Delaunay triangulation do exist (have length smaller than R) we do not need to compute sqrt((x1-x2)^2 + (y1-y2)^2) ?? R. Instead we can use the squares (x1-x2)^2 + (y1-y2)^2 ?? R^2 to avoid the square root which is slow and can lead to rounding errors. Since all input coordinates are integers and are smaller than 2^24 we know the squares are not bigger than 2^48 which still fits into the range of double or 64bit integer.

“
const int max_connected_component_size(const v2dp& planets, const int used_planets_beginning, const double squared_max_range) {
  Triangulation t(std::begin(planets)+used_planets_beginning, std::end(planets));
  for(auto v = t.finite_vertices_begin(); v != t.finite_vertices_end(); ++v){
	v->info() = false;
  }

  int max_component_size = 0;
  // Run from BFS from every unvisited node
  for(auto v = t.finite_vertices_begin(); v != t.finite_vertices_end(); ++v){
	int component_size = 0;
	std::queue<Triangulation::Vertex_handle> q;
	q.push(v);

	while(!q.empty()) {
  	const auto current_vertex = q.front();
  	q.pop();
  	if(!current_vertex->info()) {
    	component_size++;
    	current_vertex->info() = true;
    	Triangulation::Vertex_circulator c = t.incident_vertices(current_vertex);
    	do {
      	if (!t.is_infinite(c) && CGAL::squared_distance(current_vertex->point(), c->point()) <= squared_max_range){
        	q.push(c);
      	}
    	} while (++c != t.incident_vertices(current_vertex));
  	}
	}
	max_component_size = std::max(max_component_size, component_size);
  }

  return max_component_size;
}
“

**Pied Piper - DP**
Graph with a starting node and an ending node and edges with a cost inbetween. Goal is to find a maximum path from the starting node to the ending node and then back to the starting node without revisiting any nodes inbetween. Make 2D DP table:
dp[i][j] represents the maximum number of rats that can be collected by traveling from square 0 to square i along a monotonically increasing path, and then from square i to square j along a monotonically decreasing path.
“
	for(int j = 0; j < n; j++){
 	 
  	if(i == j && i != 0) continue;
  	if(dp[i][j] == -1) continue;
 	 
  	for(auto pair : upgraph[i]){
    		long target = pair.first;
    		if(target >= j){
      			dp[target][j] = std::max(dp[target][j], dp[i][j] + pair.second);
    		}
  	}
 	 
  	for(auto pair : downgraph[j]){
    		long target = pair.first;
    		if(target >= i){
      			dp[i][target] = std::max(dp[i][target], dp[i][j] + pair.second);
    		}
  	}
	}
  }
“

**Alastor Moody - Graphs + Maxflow**
You have a graph with one of the nodes being your starting position and one being the goal. Every edge has a maximum capacity and a cost/duration. We have to try and get as much flow trough the network from start to finish with every flow taking the shortest path possible. Basically we first have to calculate the shortest path from start to finish and then find all other possible paths with the same length and disregard the rest and then with the leftover graph we just push maxflow. We can calculate the graph by running Boost Djisktra from the start and the finish node; then we can read the shortest path from start to finish. Now for every node in the graph we just add the two Djisktra map values and see if it is bigger than the shortest path (discard) or the same (keep).

**Return of the Jedi - Graphs (MST)**
You have to basically compute the second best MST. First calculate MST with boost kruskal_minimum_spanning_tree. The solution for our problem can be achieved by removing one edge from the MST and adding another, different edge so that the cost of the spanning tree grows by a smallest possible value (and so that it is still a spanning tree). We can look at all the edges that are not in the spanning tree and see what is the difference between this edge and the longest edges between the two vertices (defined by the edge) using the edges of the MST. This idea is pretty simple, however, we dont know what is the longest edge on the MST between any two vertices. The main part of the solution is implementing this. We can use simple DFS. We use it N times, starting at each vertex. As we go deeper we remember the longest edge visited on that path from the starting vertex and set it into our table (for the pair starting vertex, current vertex). We iterate over all the edges that are not part of the MST. For each such edge we check what is the difference between the price of this edge and the longest edge on the path between those vertices using the edges of the MST. Note that this number is guaranteed to be positive, otherwise this edge would have to be part of the MST. We take the smallest difference and add it to the price of MST, which is the desired result.
If you find the approach mentioned above a bit too complicated you can chose an alternative approach. You can just compute the MST and then calculate additional N-1 MSTs - for each you ignore one edge of the original MST. You then use the smallest cost among the restricted MSTs. This is not only conceptually simple, but also quite simple to code and it also runs surprisingly fast.

**Schneewittchen - LP**
The problem is modeled as a tree rooted at the entry mine, with each mine contributing minerals and potentially imposing constraints if it is dangerous. To aggregate minerals and enforce constraints, a DFS is performed starting from the root. As the DFS traverses the tree, minerals available at each mine are summed with those coming from its child nodes. If a mine is dangerous, the minerals passing through it are halved, and constraints are added to ensure the total throughput does not exceed its danger threshold. This aggregation continues recursively until the entry point has a complete picture of all available minerals.
To determine the minimum cost of satisfying the mirror's mineral requirements, a linear program (LP) is constructed. Decision variables represent the amount of each mineral transported through dangerous mines (accounting for halving) and the amount purchased from the shop. The LP includes constraints to ensure mineral conservation at each node: the minerals flowing out of a node must match the minerals it produces plus what it receives from its children (adjusted for halving in dangerous mines). Additional constraints enforce the danger thresholds and ensure the total collected and purchased minerals meet the mirror’s requirements, while respecting shop supply limits. The objective function minimizes the cost of shop purchases.
Decision Variables:
xi,jx_{i,j}xi,j​: Amount of mineral jjj transported through dangerous mine iii after halving.
pjp_jpj​: Amount of mineral jjj purchased from the shop.
Constraints:
Danger Threshold: For a dangerous mine iii, the sum of incoming minerals to iii must not exceed its threshold: Total minerals entering i≤danger threshold of i.\text{Total minerals entering } i \leq \text{danger threshold of } i.Total minerals entering i≤danger threshold of i.
Conservation of Minerals: For each mine, minerals transported to the parent must equal the sum of its output plus halved inputs from dangerous children.

For every dangerous mine we basically setup this:
for every mineral j:
Variable what actually comes out of this dangerous mine X <= 0.5* ((sum of dangeorus mines below) + (Mineral we get from normal mines below))
that becomes:
2*X - (Dangerous below) <= Mineral from normal mines below

**Asterix and the Chariot Race - DP**
We basically have a tree and every node needs to either be repaired or have one connected neighboring node in either direction that gets repaired. Goal is to minimize the repair cost while ensuring all nodes are covered. Go from top to bottom (DFS) and calculate for each node 3 values:
Cost to fix this node and optimally handle all children
Cost if this node gets saved by a parent and we handle all children
Cost to save all children without saving ourselves (we need this for 
Code:
“  
long fixmecost = cost[src];
 
  for(int tar : stages[src]){
	auto [fix_child, child_saved, save_child] = solve(tar);
	long childsafe = std::min(child_saved, fix_child);
    
	fixmecost += std::min(save_child, childsafe);
	parentssave += childsafe;
	bestchild = std::min(bestchild, fix_child - childsafe);
  }
 
  return {fixmecost, parentssave + bestchild, parentssave};
“

=> bestchild is the extra cost we have to pay for one child to be repaired themselves so we get saved. So then parentssave is the cost of completely ignoring ourselves and handle all children optimall and parentssave + bestchild is the cost of getting saved by one child and handling all children optimally.
=> So a more appropriate naming for the three values would be
Fixmecost
Don’t fix me, I get fixxed by one of my children
Cost of ignoring me completely but all my children handle themselves

we need the third one only for calculating the first one.

=> result in the end is the recursive function called from the root and then taking either the first element (cost of fixing root and handle all children optimall) or second element (not fixxing root itself, but it gets saved by one child and all other children are handled optimally) (whatever is cheaper) 

**Car Sharing - MincostMaxflow**
s-nodes with n trips and each trip has a {starting point, destination point, starting time, arrival time, profit}. Sort all trips for each station according to the starting time and then connect them one after another with no cost. Then connect the source node to each stations earliest trip with the initial capacity for that station. Connect last trip to the sink. Now for every trip connect it to to the earliest possible starting time trip after the arrival time at the arrival station with it’s profit. So if a Trip arrives at station B at time 5, connect it to the next trip at station B with starting time > 5. That way we model the time constraint, since every flow/car that takes a trip will arrive at the next possible trip at the destination station and since we connected every trip in starting time oder for each station the flow/car can decide which trip to take.
Fighting Pits of Meereen - ???
Hashmap + Bit Manipulation Bullshit
“DP where state space is (north history, south history, p-q). Compute excitement from size of set that contains fighters. Then, paths are invalid if excitement is negative. Maximize the summed excitement.”

**Suez - LP**
LP problem (clear from constraints) with old poster coordinates and new power coordinates and then need to try and stretch the new posters out as much as possible given the space. 

horizontally speaking we have:
a1*w/2 + a2*w/2 = difference
=> a1 + a2 < 2*x-difference/w
then for vertically: a1 + a2 < 2*y-difference/h
then we get the constraint of:
a1 + a2 < max(2*x-difference/w, 2*y-difference/h)

**Ludo Bagman - MincostMaxflow**
Create schedule for 1v1s. Teams are divided into two groups: East and West. Matches occur between teams from opposing groups, and each team must play at least l non-difficult matches to ensure fair performance assessment. Matches have an associated risk value, with the goal being to minimize the total risk of the schedule while adhering to the constraints. A schedule consists of p matches in total though, so that’ why we might need the difficult matches. 
Just model it as a graph and connect the source to every team-east node with capacity l. Then connect every non-difficult match that can be played with it’s associated risk to the corresponding team-west node. Every team east node then gets connected to the sink with capacity l. Now in order to add the missing matches to get p in total we connect the source to an extra-node with capacity p-l*e (e being the amount of eastern teams), so basically the missing matches after having the minimum l-matches for every team. That extra node is then connected to all eastern team nodes with capacity p. For this extra flow to arrive at the sink we add another extra-node 2; then we connect every western team node to that extra2 node with capacity p. Then we add an edge from the extra2node to the sink with capacity p-l*w. Now in order to model the dangerous matches we add an extra node danger and connect the original extra node to it and then for every difficult match we add an edge from the danger node to the extra2-node. Basically we have the minimum flow l into every eastern team which then goes trough the match edges into the western node and then flow l into the sink. All extra matches to reach p go from the source to the extra node then to the eastern team, take the matches, and then flow from the western nodes into the extra node again and then into the sink.

### New ones this year
Hermione Granger
Asterix and the tour of Gaul
Hydra
Alaston Moody
Attack on kings landing
