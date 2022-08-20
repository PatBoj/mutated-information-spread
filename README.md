# Topic selectivity and adaptivity promote spreading of short messages
## Files
Source Java files are located in the `src` folder. Different packages are responsible for various tasks:
* `src/Networks` is responsible for creating (or loading existing) networks. It is possible to make different models - BA, ER or custom network;
* `src/ProgramingTools` is used for debugging and computing time of execution;
* `src/Dynamics` contains all information about dynamics in the model;
* `src/Main` main executive files. In the `Experiments` file, we are setting all parameters. It also contains other simulation scenarios (like running with no competition).
All results are saved in the `results` folder. A few `.R` files are responsible for creating plots. Also, real networks' topologies are stored in the `networks` directory.

## Parameters used
All parameters can be modified in the `src/Main/Experiments.java` file:
* `int N` size of the network;
* `int k` average node degree;
* `int timeSteps` time steps after which simulation stops;
* `int dimOpinion` length (number of different topics) of the opinion vector;
* `double pEdit` probability of editing a message in a single step;
* `double pNewMessage` probability of creating a new message in one time step;
* `int realisations` number of independent realisations;
* `double threshold` cosine threshold after which message is 'liked' by an agent;
* `String topologyType` set BA or ER for different topology models.
