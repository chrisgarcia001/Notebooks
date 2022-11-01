# Multi-Depot Last Mile Delivery

This notebook demonstrates how to use integer programming to solve the Multi-Depot Last Mile Delivery problem with a 
heterogeneus vehicle fleet. In this problem there are multiple depots from which trucks can fulfill customer orders, and 
customer orders can be split between multiple depots when advantageous or necessary. Additionally, each truck
may have a different capacity and cost per mile. The objective is to minimize total delivery costs by jointly 
determining which items should be shipped to each customer from each depot, which trucks should go to which
customers, and which route each truck should take.

We show how to model, solve, and visualize the optimal routes in Python using 
the [PuLP](https://coin-or.github.io/pulp/)  and [VeRoViz](https://veroviz.org/) packages.
