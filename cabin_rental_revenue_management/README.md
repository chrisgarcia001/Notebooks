# Optimizing Vacation Cabin Rental Revenues

This notebook demonstrates how to use one aspect of Revenue Management, optimal capacity allocation, to maximize 
revenues attained by renting out vacation cabins over a long weekend. In this example we have 3 small cabins, 5 medium cabins, 
and 3 large cabins that we can rent out. We are also provided with a demand forecast and best prices for the
available rental slots over the weekend. We compare two integer programming-based optimization policies (one that allows larger cabins to
substitute for smaller ones when advantageous, and another that does not) against a human-like, first-come-first-served
policy to demonstrate the impact RM can have on revenues. 


This example uses the [PuLP](https://coin-or.github.io/pulp/) package modeling and solving the integer programming 
optimization model.
