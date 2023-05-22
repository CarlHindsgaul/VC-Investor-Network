## TEST STARTS AT AROUND LINE 150, REST IS JUST REQUIREMENT TO RUN THE SCRIPT

### load packages (install first if the PC doesn't have them installed)

library(tidyverse)
library(readxl)
library(stringr)
library(igraph)
library(openxlsx)
library(modelsummary)
library(broom)

### load data

### find the VC Data file on my github: https://github.com/CarlHindsgaul/VC-Investor-Network)
### now download it and modify the path in the script so your downloaded file is loaded as the dataframe

data <- read_excel("C:\\Users\\carlw\\Mit drev\\2nd Year Project\\VC Data.xlsx")

### restructure data so each row represents an "edge" (connection between investor and company)

edge_list <- data %>%
  mutate(Investors = strsplit(as.character(Investors), ", ")) %>%
  unnest(Investors) %>% 
  separate(Investors, c("Investor", "InvestorCountry"), " \\(") %>% 
  mutate(InvestorCountry = gsub("\\)", "", InvestorCountry))


### restructure edge_list to an adjacency matrix, which is useful for network analysis

### first make a new dataframe with only edgelist variables (company and investor)

edge_list_excl <- edge_list %>%
  select(Investor, Company)

### now make an incidence matrix. We need to create an igraph object, i.e. a network-analysis object. 
### the iGraph object requires a list of nodes and the type of each node.

### we only want the unique names. In the edge_list, each company and investor (each 'vertex') can appear several times.

vertex_list <- unique(c(edge_list_excl$Company, edge_list_excl$Investor))

### we also want to have information about whether each node/name is a company or an investor.

vertex_types <- ifelse(vertex_list %in% edge_list_excl$Company, TRUE, FALSE)

### we now create the igraph object, let us call it "g", and convert it into an incidence matrix.

g <- graph_from_data_frame(edge_list_excl, directed = FALSE, vertices = data.frame(name = vertex_list, type = vertex_types))

incidence_matrix <- as_incidence_matrix(g, sparse = FALSE)

### Make an adjacency matrix
### (First, we turn the incidence matrix into a binary, so only 1 for investment, otherwise 0. This is to prepare for later calculations that require binary format)

binary_investment_matrix <- as.matrix(ifelse(incidence_matrix > 0, 1, 0))

### (Second, we multiply (%*%) the binary investment matrix with its transpose (i.e. 'turned' matrix, so row 1 becomes column 1 etc.))
### (The result is the 'overlap' of investors since we have binary data, only 1 and 0, and matrix multiplication works by multiplying row values with column values
###  so when, say, investor row b has a '1' on its fourth column, representing company y, and transpore-investor a has a 1 in its fourth row, we have 1*1 = an overlap
###  and the way matrix multiplication works, all row & column values are 'compared' like this and summed up, thus counting overlaps)
### All right, so we do the calculation: multiply the matrix with its transpose (t() function)

co_investment_matrix <- binary_investment_matrix %*% t(binary_investment_matrix)

### But we also set the diagonal elements to 0, since we are not concerned with how connected each fund is with itself

diag(co_investment_matrix) <- 0

### Now we have the adjacency matrix. I will just create a separate object for it with that name: adjacency_matrix
### I will write it to csv and upload it to github 

adjacency_matrix <- co_investment_matrix

### With the adjacency matrix, which is useful for network analysis, we can make the igraph i.e. network analysis object

g2 <- graph_from_adjacency_matrix(adjacency_matrix, mode = "undirected")

### We can now, for the first time, get a visual, qualitative sense of the data!
### (it gets too messy with all the labels)

plot(g2,
     vertex.size = 7,
     vertex.label = NA,
     main = "Investor Network / no label")

### Ok great, now let us create network data for the investors: betweenness centrality, centralization score, and k-step reach.

### we first need a dataset if we want to make a quantitative model that uses all of these measures at the same time. We will make a new one called edge_list_2.

edge_list_2 <- edge_list

### we then start with betweenness

betweenness_object <- betweenness(g2)

betweenness_df <- data.frame(
  Investor = names(betweenness_object),
  Betweenness = betweenness_object)

edge_list_2 <- merge(edge_list_2, betweenness_df, by = "Investor", all.x = TRUE) ### the last part all.x is about keeping all rows, and by = means we use Investor column for 'matching' values to the appropriate investor

### let us now go to centralization score

cent_degree_object <- degree(g2,
                             loops = FALSE,
                             normalized = FALSE)

cent_degree_df <- data.frame( 
  Investor = names(cent_degree_object),
  Degree_centralization = cent_degree_object)

edge_list_2 <- merge(edge_list_2, cent_degree_df, by = "Investor", all.x = TRUE)

### Now we have added the raw degree centralization (number of connections)
### let us add a variable that normalizes this by dividing the centralization with the highest ACTUAL (not highest theoretical) degree centralization

edge_list_2$Normalized_degree_centralization <- edge_list_2$Degree_centralization / 44    ### We use 44 because 44 is the highest degree centralization (Seed Capital)


### Now let us go to transitivity 
### We use the local transitivity function: "The local transitivity of a vertex is the ratio of the count of triangles connected to the vertex and the triples centered on the vertex."

transitivity_object <- transitivity(g2,
                                    type = "localundirected",
                                    isolates = "zero")

Investor_names <- unique(edge_list_2$Investor)

transitivity_df <- data.frame(
  Investor = Investor_names,
  Transitivity = transitivity_object)

edge_list_2 <- merge(edge_list_2, transitivity_df, by = "Investor", all.x = TRUE)


### Now let us go to K-step reach, with K = 2 (so unique nodes accessible via 2 steps, e.g. A=B and B=C, then A has a K2 reach of 2)

investor_indices = which(V(g2)$name %in% edge_list_2$Investor)

k_step_reach_list <- ego(g2, order = 2, mode = "all", mindist = 1, nodes = investor_indices)

k_step_reach_counts <- sapply(k_step_reach_list, function(x) length(x))

k_step_reach_df <- data.frame(
  Investor = V(g2)$name[investor_indices],
  K_step_reach = k_step_reach_counts)

edge_list_2 <- merge(edge_list_2, k_step_reach_df, by = "Investor", all.x = TRUE)






### I will test out what the network looks like with binary edges (never more than 1 connection to any investor) ###

binary_matrix <- adjacency_matrix

binary_matrix[binary_matrix > 0] <- 1

gbinary <- graph_from_adjacency_matrix(binary_matrix, mode = "undirected")

plot(gbinary,
     vertex.size = 7,
     vertex.label = NA,
     main = "Investor Network / Binary Ties")

### I will also test degree centralization with binary edges/no edge

binary_cent_degree_object <- degree(gbinary,
                                    loops = FALSE,
                                    normalized = FALSE)

binary_cent_degree_df <- data.frame( 
  Investor = names(binary_cent_degree_object),
  Binary_degree_centralization = binary_cent_degree_object)

edge_list_2 <- merge(edge_list_2, binary_cent_degree_df, by = "Investor", all.x = TRUE)

edge_list_2$Binary_normalized_degree_centralization <- edge_list_2$Binary_degree_centralization / 37    ### We use 37 because 37 is the highest UNIQUE degree centralization (Seed Capital)

## I will illustrate the difference between controlling for multiple investments in the normalized centralization score: 

ggplot(edge_list_2) +
  geom_density(aes(x = Binary_normalized_degree_centralization, color = "Binary"), size = 1) +
  geom_density(aes(x = Normalized_degree_centralization, color = "Normalized"), size = 1) +
  labs(title = "Binary vs Normalized Degree Centralization",
       x = "Centralization Score",
       y = "Density",
       color = "Calculation Methods") +
  theme_classic()

### Test over ###

