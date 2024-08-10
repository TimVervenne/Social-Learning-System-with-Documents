library(igraph)
library(colorRamps)
library(extraDistr)
library(matrixStats)
library(gplots)

# Function to create a rooted tree which represents a cultural system
generate_rooted_tree_branching <- function(num_skill_paths, path_depth, num_nodes) {
  # Create an empty graph without any adges
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:num_skill_paths) {
    edgeList<-c(edgeList, 1, i+1)
    
    j<-i+1
    for (k in 1:(path_depth-1)) {
      edgeList<-c(edgeList, j, j+(num_skill_paths))
      j<-j+num_skill_paths
    }
  }
  g<-add_edges(g, c(edgeList))
  return(g)
}

# Function to determine the start nodes of which paths will be unavailable in the (first) environmental shift
unavailable_start_nodes<-function(num_skill_paths,num_paths_to_exclude){
  start_nodes_paths<-sample(c(2:num_skill_paths+1), num_paths_to_exclude)
  return(start_nodes_paths)
}

# Function to determine the start nodes of which paths will be unavailable in the (subsequent) environmental shift
reverse_start_nodes<-function(start_nodes_paths){
  new_nodes<-2:(num_skill_paths+1)
  for (i in start_nodes_paths){
    new_nodes<-new_nodes[-which(new_nodes==i)]
  }
  start_nodes_paths<-new_nodes
  return(start_nodes_paths)
}

# Function to make the chosen trait paths unavailable to be learned or invented
make_traits_unavailable<- function(num_skill_paths, path_depth, num_nodes, start_nodes_paths) {
  available_skills<-rep(1, num_nodes)
  for (i in start_nodes_paths) {
    available_skills[i] = 0
    for (j in 1:(path_depth - 1)) {
      available_skills[i + (j * num_skill_paths)] = 0
    }
  }
  return(available_skills)
}

# Function to initialize the population at the start of a simulation
initializePopulation<-function(N, num_nodes, adj_matrix){
  ## start with empty repertoires (but fill them up in the next step)
  repertoires<-matrix(0, nrow=N, ncol=num_nodes)
  ## everyone has the root trait of the cultural system
  repertoires[,1]<-1
  
  return(repertoires)
}

# Function to initialize which of the agents will be documents in the simulation
initializeDocuments<-function(num_agents, num_doc) {
  documents<-c(rep(0,num_agents),rep(1,num_doc))
  return(documents)
}

distribute_learning_strategies<-function(N,num_agents){
  learning_strategies_agents<-rep(0,N)
  vec_learning_strategies<-c(rep(1, floor(num_agents*0.25)), rep(2, floor(num_agents*0.25)),rep(3, ceiling(num_agents*0.25)), rep(4, ceiling(num_agents*0.25)))
  for (i in 1:num_agents){
    learning_strategies_agents[i]<-sample(vec_learning_strategies,1,replace=F)
  }
  return(learning_strategies_agents)
}

# Function to assign ages to individuals in the simulation at the start of the simulation
assignAges<-function(N, max_age){
  popAge<- rep(0,N)
  for (ind in 1:N) {
    popAge[ind]<-sample(1:max_age,1)
  }
  return (popAge)
  
}

# Function to obtain the traits that are learnable to an agent
getLearnableTraits<-function(repertoires, ind, adj_matrix){
  # which traits are currently not in the individual's repertoire?
  unknownTraits<-which(repertoires[ind,]==0)
  
  # for which of these traits is the parent trait in the repertoire?
  # these are the traits currently 'learnable' to the individual
  learnableTraits<-c()
  for (trait in unknownTraits){
    parent<-which(adj_matrix[,trait]==1)
    if (prod(repertoires[ind,parent]==1)){
      learnableTraits<-c(learnableTraits,trait)} 
  }
  
  return(learnableTraits)
}

# Function to attempt to obtain a trait through observing another agent
learnSocially <- function(repertoires, ind, adj_matrix, M, learningStrategy, popAge, payoffs, num_agents){
  ## sample M random other individuals
  pool<-1:N
  pool_noDocuments<-1:num_agents
  poolOthers<-pool[-ind] # agents do not sample themselves
  poolOthers_no_docs<-pool_noDocuments # documents can only sample agents

  if (documents[ind]==0){
    models<-sample(poolOthers, M, replace=FALSE)
  }
  if (documents[ind]==1){
    models<-sample(pool_noDocuments, M, replace=FALSE)
  }
  ## randomly pick 1 trait from each model (mogelijk aanpassen, weet niet of 1 trait realistisch is)
  ## only consider traits the learning agent do not know yet
  observedBehaviours<-c()
  observedModels<-c()
  for (model in models){
    newTraits<-c()
    for (k in 1:num_nodes){
      if (repertoires[model,k]==1 && repertoires[ind,k]==0) {
        newTraits<-c(newTraits,k)
      }
    }
    if (length(newTraits)>1) {
      tr<-sample(newTraits,1)
      observedBehaviours<-c(observedBehaviours, tr)
      observedModels<-c(observedModels, model)
    }
    if (length(newTraits)==1) {
      observedBehaviours<-c(observedBehaviours, newTraits)
      observedModels<-c(observedModels, model)
    }
  }
  
  ## check if the vectors are filled as expected
  observedBehaviours
  observedModels
  
  ## if there's no trait to learn among the observed ones, skip
  ## SLpayoffs was initialized at NA so nothing happens in that case
  ## if there IS something to learn:
  if (length(observedBehaviours)>0){
    ## list of weights of each observed behaviour
    ## weights depend on learning strategy
    ## normalized weights are used for choice
    wList<-c()
    selectedTrait<-c()
    
    ######	STRATEGY 0: random learning benchmark #####				
    ## select a trait you dont have at random
    if (learningStrategy==0){
      ## select a trait you dont have at random
      wList<-rep(1,length(observedBehaviours))							
    }
    
    ######	STRATEGY 1: conformist social learning #####				
    ## count the selected behaviours and weigh common ones more
    if (learningStrategy==1){
      ## count the selected behaviours and weigh common ones more
      for (mod in 1:length(observedBehaviours)){
        w<- length(which(observedBehaviours==observedBehaviours[mod]))
        wList<-c(wList, w)
      }			
    }			
    
    ######	STRATEGY 2: age-based social learning #####				
    if (learningStrategy==2){
      ## based on age similarity
      ## check for all agents how similar they are to self
      for (mod in observedModels){
        w<- 10^-8
        ageDif<-popAge[mod]-popAge[ind]								
        #if (ageDif >=0) w<- 0.5^ageDif
        #if (ageDif >=0) 
        w<-(10^-8) + dnorm(ageDif, mean=0, sd=2)
        wList<-c(wList, w)
      }			
    }
    
    ###### STRATEGY 3: similarity based learning ######
    if (learningStrategy==3){
      ## Check for all agents how similar they are in trait overlap
      for (mod in observedModels){
        simToFocal<-0
        for (k in 1:num_nodes){ ## loop over all traits and sum similarity
          if (repertoires[ind,k]==repertoires[mod,k]) simToFocal<-simToFocal+1;
        }
        wList<-c(wList, simToFocal)
      }
    }
    
    ##### STRATEGY 4: payoff-based social learning #####
    if (learningStrategy==4){
      wList<-payoffs[observedBehaviours]/sum(payoffs[observedBehaviours])
    }
    
    ### MAKE CHOICE ###
    selectedTrait<-ifelse(length(observedBehaviours)==1, observedBehaviours[1], sample(observedBehaviours,1,prob=wList))
    #		if (length(observedBehaviours)==1) {selectedTrait<-observedBehaviours[1]}
    #		else {selectedTrait<-sample(observedBehaviours, 1, prob=wList)}
    return(selectedTrait)	
  }						
}

## if showPopState==1, vary node size with portion of agents with that trait
plotTree <- function(tree, repertoires, showPopState) {	
  # Use layout_as_tree to create a tree layout
  layout <- layout_as_tree(tree, root=1, rootlevel=0)
  
  # Calculate the x-coordinates to center the tree horizontally
  levels <- distances(tree, v = 1, to = V(tree), mode = "out")
  level_widths <- table(levels)
  
  # Calculate the total width of the tree
  total_width <- max(level_widths)
  mid_x<-total_width/2
  x_coordinates<-c()
  for (level in level_widths) {
    x0<-mid_x
    for (k in 1:level){
      x<- (k-0.5) / level
      x_coordinates<-c(x_coordinates, x)
    }
  }
  # Set the x-coordinates in the layout
  layout[, 1] <- x_coordinates
  
  # set colouring according to depth
  nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
  colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
  color_palette <- colramp(max(nodeDepths))
  color <- color_palette[1:num_nodes]
  V(tree)$color <- color_palette[nodeDepths]
  
  ## plot the tree
  if (showPopState==0) {
    plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
         vertex.color = V(tree)$color, vertex.label=NA)
  }
  if (showPopState==1){
    V(tree)$propAdopted <- rep(0,num_nodes)
    for (tr in 2:num_nodes) V(tree)$propAdopted[tr]<-mean(repertoires[,tr])
    plot(tree, layout = layout, vertex.size=30*V(tree)$propAdopted, edge.arrow.size = 0.5, edge.color='black',
         vertex.color = V(tree)$color, vertex.label=NA)		
  }
}

meansplot_simulations<-function(data,title,xlabel,ylabel){
  y<-rep(NA,timesteps*replicateSimulations)
  x<-rep(NA,timesteps*replicateSimulations)
  for (i in 1:replicateSimulations){
    y[(((i-1)*timesteps)+1):(i*timesteps)]<-data[[i]]
    x[(((i-1)*timesteps)+1):(i*timesteps)]<-rep(i,timesteps)
  }
  meansplot<-plotmeans(y~x,main=title, xlab=xlabel, ylab=ylabel)
  return(meansplot)
}

meansplot_replicates<-function(data,simulation_number,title,xlabel,ylabel){
    y<-rep(NA,timesteps*replicateSimulations)
    x<-rep(simulation_number,timesteps*replicateSimulations)
    for (i in 1:replicateSimulations){
        y[(((i-1)*timesteps)+1):(i*timesteps)]<-data[[i]]
    }    
    meansplot<-plotmeans(y~x,main=title, xlab=xlabel, ylab=ylabel)
    return(meansplot)
}

#### simulation parameters - basic
num_skill_paths = 4			# amount of different skill paths, must be an even number
path_depth = 9				# depth of the skill paths
N<-1000						# population size - between 100 / 1000 for "predictable" and fast results
num_doc<-0					# number of documents in the system
M<-10						# number of demonstrators
max_age<-100                # maximum age an agent can acquire
timesteps<-1000				# number of time steps in the simulation
replicateSimulations<-40	# number of simulations per parameter setting
simulationAmount<-30
S<-0.99						# reliance on social learning; (1-S) is innovation rate
IS_base<-0.1					# chance of innovation success - rond 0.10 á 0.15?
SS_base<-0.5         			# social learning chance - tussen 0.5 en 1 voor realisme
reset_rate_base<- 5 / N			# probability that an individual is replaced by a naive individual - absolute frequency of the population that are averagely reset each timestep - let it be between 5-10% of population size
reset_rate_documents_base<- 2 / timesteps    #probability than a document perishes and is replaced by a blank document 

IS_shift<-IS_base / 1
SS_shift<-SS_base / 1
reset_rate_shift<-reset_rate_base * 10

learningStrategy<-0         # learning strategy used by agents, 0 = random, 1 = conformist, 2 = age-based, 3 = similarity-based, 4 = payoff

# parameters for plotting (number of time steps per printing event frequencies)
windowSize<-1000

### define the cultural system ###
## generate rooted tree to represent the cultural system
num_paths_to_exclude<-num_skill_paths/2         # number of paths to exclude, depending on amount of environmental shifts
num_nodes = num_skill_paths*path_depth+1				# size of the cultural systems (number of nodes)		
tree <- generate_rooted_tree_branching(num_skill_paths, path_depth, num_nodes)
nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
maxNodeDepth<-max(nodeDepths)
#plotTree(tree,0,0)

## derive square matrix of parent/child traits
adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
adj_matrix[1,1]<-1  ## root trait (at position 1,1) is its own parent

# When running multiple simulations
#simulationAmount<-30
replicate_num_doc<-c(rep(0,5),rep(1,5),rep(10,5),rep(50,5),rep(100,5),rep(500,5))
#replicate_M<-c(1,5,1,5)
#replicate_N<-c(10,100,1000,10000)
#replicate_path_depth<-c(9,9,3,3)
#replicate_num_skill_paths<-c(4,4,12,12)
#replicate_SS<-c(0.05,0.2,0.35,0.5)
#replicate_IS<-c(0.05,0.2,0.35,0.5)
#replicate_reset_rate_base<-c(10,50,10,50)
#replicate_reset_rate_documents_base<-c(0,1,2,4)
#replicate_SS_shift<-c(5,10,5,10)
#replicate_IS_shift<-c(5,10,5,10)
#replicate_reset_rate_shift<-c(1,2,5,10)
replicate_learningStrategy<-rep(c(0,1,2,3,4),6)

overall_meanTraitsInSystem<-rep(NA,simulationAmount)
overall_meanTraitsAgents<-rep(NA,simulationAmount)
overall_meanCumulativePayoffsAgents<-rep(NA,simulationAmount)
overall_meanCumulativePayoffsAgents_relative<-rep(NA,simulationAmount)
overall_SLrelativesuccess<-rep(NA,simulationAmount)
overall_SLrelativesuccessagents<-rep(NA,simulationAmount)
overall_meanSLpayoffs<-rep(NA,simulationAmount)
overall_meanSLpayoffs_relative<-rep(NA,simulationAmount)
overall_meanSLpayoffsAgents<-rep(NA,simulationAmount)
overall_meanSLpayoffsAgents_relative<-rep(NA,simulationAmount)
overall_mean_total_skills_in_system<-rep(NA,simulationAmount)
overall_mean_total_skills_with_agents<-rep(NA,simulationAmount)

df_overall_meanTraitsInSystem_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanTraitsAgents_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanCumulativePayoffsAgents_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanCumulativePayoffsAgentsRelative_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLrelativeSuccess_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLrealtiveSuccessAgents_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLpayoffs_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLpayoffsAgents_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLpayoffsRelative_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSLpayoffsAgentsRelative_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSkillsInSystem_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)
df_overall_meanSkillsWithAgents_replicates<-matrix(nrow=simulationAmount, ncol=replicateSimulations)

for (simulation in 1:simulationAmount){

    ##### different parameter values for different simulations
    #path_depth<-replicate_path_depth[simulation]	
    #num_skill_paths<-replicate_num_skill_paths[simulation]
    num_doc<-replicate_num_doc[simulation]
    #N<-replicate_N[simulation]
    #M<-replicate_M[simulation]
    #SS<-replicate_SS[simulation]
    #IS<-replicate_IS[simulation]
    #reset_rate_base<-replicate_reset_rate_base[simulation] / N
    #reset_rate_documents_base[simulation]<-replicate_reset_rate_documents_base / timesteps
    #IS_shift<-IS_base / replicate_IS_shift[simulation]
    #SS_shift<-SS_base / replicate_SS_shift[simulation]
    #reset_rate_shift<-reset_rate_base*replicate_reset_rate_shift[simulation]
    learningStrategy<-replicate_learningStrategy[simulation]

    # allocate variables
    sim_meanTraitsInSystem<-list()
    sim_meanTraitsAgents<-list()
    sim_varTraitsInSystem<-list()
    sim_skills_in_system<-list()
    sim_total_skills_in_system<-list()
    sim_total_skills_with_agents<-list()
    sim_skill_learned_per_timestep<-list()
    sim_num_traits_individuals_per_timestep<-list()
    sim_trait_times_invented<-list()
    sim_trait_times_learned<-list()

    sim_times_SL<-list()
    sim_times_IN<-list()
    sim_times_SL_agents<-list()
    sim_times_SL_success<-list()
    sim_times_IN_success<-list()
    sim_times_SL_success_agents<-list()
    sim_times_SL_relative_success<-list()
    sim_times_IN_relative_success<-list()
    sim_times_SL_relative_success_agents<-list()

    sim_payoffs<-list()
    sim_SLpayoffs<-list()
    sim_meanSLpayoffs<-list()
    sim_meanSLpayoffsAgents<-list()
    sim_relative_meanSLpayoffs<-list()
    sim_relative_meanSLpayoffsAgents<-list()
    sim_meanCumulativePayoffsAgents<-list()
    sim_meanCumulativePayoffsAgents_relative<-list()

    sim_meanSLpayoffs_raw<-list()
    sim_meanSLpayoffsAgents_raw<-list()
    sim_relative_meanSLpayoffs_raw<-list()
    sim_relative_meanSLpayoffsAgents_raw<-list()

    ### PARAMETERS ARE SET ###
    for (repl in 1:replicateSimulations){
        ## show simulation progress on screen
        flush.console()
        
        ####### INITIALIZE POPULATION #######
        # Set payoffs
        #payoffs_base<-rep(1,num_nodes)                                          # fixed payoff
        #payoffs_base<-runif(num_nodes)                                          # random payoffs from uniform distribution
        payoffs_base<-runif(num_nodes) * nodeDepths                             # payoffs increase with depth
        #payoffs_base<-runif(num_nodes) * (max(nodeDepths) - nodeDepths + 1)     # payoffs decrease with depth
        
        # Allocate variables for the simulation
        meanTraitsInSystem<-rep(NA,timesteps) 	## mean number of traits in the system
        meanTraitsAgents<-rep(NA,timesteps)   ## mean number of traits acquired by agents
        varTraitsInSystem<-rep(NA,timesteps) ## variation in number of traits in the system
        total_skills_in_system<-rep(NA,timesteps)
        total_skills_with_agents<-rep(NA,timesteps)
        skills_in_system<-matrix(nrow=timesteps,ncol=num_nodes)
        skills_with_agents<-matrix(nrow=timesteps,ncol=num_nodes)
        skill_learned_per_timestep<-matrix(nrow=timesteps,ncol=num_nodes)
        num_traits_individuals_per_timestep<-matrix(nrow=timesteps,ncol=N)
        trait_times_invented<-rep(0,num_nodes)
        trait_times_learned<-rep(0,num_nodes)
        trait_times_invented_per_timestep<-matrix(nrow=timesteps,ncol=num_nodes)
        trait_times_learned_per_timestep<-matrix(nrow=timesteps,ncol=num_nodes)
        
        times_learning<-rep(0,timesteps)
        times_SL<-rep(0,timesteps)
        times_IN<-rep(0,timesteps)
        times_SL_success<-rep(0,timesteps)
        times_IN_success<-rep(0,timesteps)
        times_SL_relative_success<-rep(0,timesteps)
        times_IN_relative_success<-rep(0,timesteps)
        times_SL_agents<-rep(0,timesteps)
        times_SL_success_agents<-rep(0,timesteps)
        times_SL_relative_success_agents<-rep(0,timesteps)
        
        SLpayoffs<-matrix(nrow=timesteps,ncol=N)
        meanSLpayoffs<-rep(0,timesteps)
        meanSLpayoffsAgents<-rep(0,timesteps)
        relative_meanSLpayoffs<-c()
        relative_meanSLpayoffsAgents<-c()
        meanCumulativePayoffsAgents<-rep(0,timesteps)
        meanCumulativePayoffsAgents_relative<-rep(0,timesteps)
        
        ####### INITIALIZE POPULATION #######
        num_agents<-N-num_doc		# number of agents in the system
        documents<-initializeDocuments(num_agents, num_doc)
        learning_strategies_agents<-distribute_learning_strategies(N, num_agents)
        available_traits<-rep(1,num_nodes)
        start_nodes_paths<-unavailable_start_nodes(num_skill_paths,num_paths_to_exclude) ## set the nodes that will be unavailable in the environmental change
        repertoires<-initializePopulation(N, num_nodes, adj_matrix)
        popAge<-assignAges(N,max_age)
        payoffs_earned<-rep(0,num_agents)
        payoffs_earned_relative<-rep(0,num_agents)
        learningTrait<-c()
        learningTraitAvailable<-FALSE
        selectedTraitAvailable<-FALSE

        ### population is now initialized... start running the model
        for (t in 1:timesteps){
            if (t == 1){
                reset_rate<-reset_rate_base
                reset_rate_documents<-reset_rate_documents_base
                SS<-SS_base
                IS<-IS_base
                payoffs<-payoffs_base
            }
            
            if (t == round(.2*timesteps)+1){
                reset_rate<-reset_rate_shift
                SS<-SS_shift
                IS<-IS_shift
            }
            if (t == round(.4*timesteps)+1) {
                available_traits<-make_traits_unavailable(num_skill_paths, path_depth, num_nodes, start_nodes_paths)
                for (i in available_traits){
                    payoffs[i]<-0
                }
            }
            if (t == round(.6*timesteps)+1) {
                payoffs<-payoffs_base
                start_nodes_paths<-reverse_start_nodes(start_nodes_paths)
                available_traits<-make_traits_unavailable(num_skill_paths, path_depth, num_nodes, start_nodes_paths)
                for (i in available_traits){
                    payoffs[i]<-0
                }
            }
            if (t == round(.8*timesteps)+1) {
                available_traits<-rep(1,num_nodes)
                payoffs<-payoffs_base
                reset_rate<-reset_rate_base
                SS<-SS_base
                IS<-IS_base
            }
            
            #We go through all the individuals
            for (ind in 1:N) {
                # At the beginning of each timestep, the agent ages with 1
                popAge[ind]<-popAge[ind]+1
                if (documents[ind] == 0){
                    payoffs_earned[ind]<-0
                    payoffs_earned_relative[ind]<-0
                    for (i in 1:num_nodes){
                        if (available_traits[i] == 1 && repertoires[ind,i] == 1){
                            payoffs_earned[ind]<-payoffs_earned[ind]+payoffs[i]
                        }
                    }
                    payoffs_earned_relative[ind]<-payoffs_earned[ind]/sum(payoffs)
                }
        
                #if (documents[ind] == 0){
                #  learningStrategy<-learning_strategies_agents[ind]
                #}

                if (documents[ind] == 1){ # documents always have the random learning strategy
                    learningStrategy<-0
                }
            
                if (round(ind) == round(N / 2)) {
                    cat(paste('simulation = ',simulation, ',replicate = ',repl,', timestep = ',t,', individual = ',ind,'\n'))
                    cat('repertoire = ',repertoires[ind,],'\n')
                }
            
                if (t != 1 && learningTraitAvailable == TRUE && is.na(learningTrait[ind]) == FALSE){
                    repertoires[ind,learningTrait[ind]]<-1
                }
                ## Individual is replaced by a naive individual at random (dying due to environment)
                if (runif(1) < reset_rate && documents[ind] == 0) {
                    repertoires[ind,]<-c(1,rep(0,num_nodes-1))
                    popAge[ind]<-0
                }
            
                ## Replace an dying individual with a young individual (dying of natural causes)
                if (popAge[ind]>= 100 && documents[ind] == 0) {
                    repertoires[ind,]<-c(1,rep(0,num_nodes-1))
                    popAge[ind]<-0
                }
            
                if (runif(1) < reset_rate_documents && documents[ind]== 1){
                    repertoires[ind,]<-c(1,rep(0,num_nodes-1))
                    popAge[ind]<-0
                }

                ## will they learn individually or socially?
                r<-runif(1)
                p<-runif(1)
                q<-runif(1)
            
                selectedTraitAvailable<-FALSE
                learningTraitAvailable<-FALSE
                learnableTraits<-getLearnableTraits(repertoires, ind, adj_matrix)
            
                if (length(learnableTraits)>0){  #only try to learn if there's anything to learn
                    times_learning[t]<-times_learning[t]+1
                    if (r<S) {  # social learning
                        times_SL[t]<-times_SL[t]+1
                        if (ind <= num_agents){
                            times_SL_agents[t]<-times_SL_agents[t]+1
                        }
                        if (q < SS){
                            selectedTrait<-learnSocially(repertoires, ind, adj_matrix, M, learningStrategy, popAge, payoffs, num_agents)
                            if (available_traits[selectedTrait] == 1 && selectedTrait %in% learnableTraits == TRUE && is.null(selectedTrait) == FALSE) {
                                selectedTraitAvailable<-TRUE
                                times_SL_success[t]<-times_SL_success[t]+1
                                if (ind <= num_agents){
                                    times_SL_success_agents[t]<-times_SL_success_agents[t]+1
                                }
                                SLpayoffs[t,ind]<-payoffs[selectedTrait]
                                trait_times_learned[selectedTrait]<-trait_times_learned[selectedTrait]+1
                            }
                        }
                    }
                
                    else if (documents[ind] == 0) {	# individual learning (= innovation)
                        times_IN[t]<-times_IN[t]+1
                        if(p<IS){
                            selectedTrait<-learnableTraits[1]
                            if (length(learnableTraits)>1) {
                                selectedTrait<-sample(learnableTraits,1)
                                if (available_traits[selectedTrait] == 1 && is.null(selectedTrait) == FALSE){
                                    selectedTraitAvailable<-TRUE
                                    times_IN_success[t]<-times_IN_success[t]+1
                                    trait_times_invented[selectedTrait]<-trait_times_invented[selectedTrait]+1
                                }
                            }
                        }
                    }
                
                    # If document and no social learning, we havent gained a skill
                    if (selectedTraitAvailable == TRUE) {
                        learningTrait[ind]<-selectedTrait
                        learningTraitAvailable<-TRUE
                    }
                }
            }
            
            # BOOKKEEPING for each time step
            # number of traits in the system
            meanTraitsInSystem[t]<-sum(repertoires) / (num_nodes * N)
            meanTraitsAgents[t]<-sum(repertoires[1:num_agents,]) / (num_nodes * num_agents)
            varTraitsInSystem[t]<-mean(colVars(repertoires))
            for (i in 1:num_nodes){
                if (sum(repertoires[,i])==0){
                    skills_in_system[t,i]<-0
                } else {
                skills_in_system[t,i]<-1
                }
                if (sum(repertoires[1:num_agents,i])==0){
                    skills_with_agents[t,i]<-0
                } else {
                    skills_with_agents[t,i]<-1
                }
            }
            total_skills_in_system[t]<-sum(skills_in_system[t,])
            total_skills_with_agents[t]<-sum(skills_with_agents[t,])
            for (i in 1:num_nodes){
                trait_times_invented_per_timestep[t,i]<-trait_times_invented[i]
            }
            trait_times_learned_per_timestep[t,]<-trait_times_learned
            # number of individuals having learnt a trait per timestep
            for (i in 1:num_nodes){
                skill_learned_per_timestep[t,i]<-sum(repertoires[,i])	
            }	
            # amount of skills the individuals / documents have learned per timestep
            for (i in 1:N){
                num_traits_individuals_per_timestep[t,i]<-sum(repertoires[i,])
            }
            # payoffs
            meanSLpayoffs[t]<-mean(na.omit(SLpayoffs[t,]))
            meanSLpayoffsAgents[t]<-mean(na.omit(SLpayoffs[t,1:num_agents]))
            meanCumulativePayoffsAgents[t]<-mean(payoffs_earned)
            meanCumulativePayoffsAgents_relative[t]<-mean(payoffs_earned_relative)
        }
    
        ## Final simulation calculations
        # relative learning success
        times_IN_relative_success<-times_IN_success/times_IN
        times_SL_relative_success<-times_SL_success/times_SL
        times_SL_relative_success_agents<-times_SL_success_agents/times_SL_agents
        relative_meanSLpayoffs<-meanSLpayoffs*num_nodes/sum(payoffs)
        relative_meanSLpayoffsAgents<-meanSLpayoffsAgents*num_nodes/sum(payoffs)

        ## bookkeep for summary across simulation replicates
        sim_meanTraitsInSystem[[repl]]<-meanTraitsInSystem
        sim_meanTraitsAgents[[repl]]<-meanTraitsAgents
        sim_varTraitsInSystem[[repl]]<-varTraitsInSystem
        sim_skills_in_system[[repl]]<-skills_in_system
        sim_total_skills_in_system[[repl]]<-total_skills_in_system
        sim_total_skills_with_agents[[repl]]<-total_skills_with_agents

        sim_skill_learned_per_timestep[[repl]]<-skill_learned_per_timestep
        sim_num_traits_individuals_per_timestep[[repl]]<-num_traits_individuals_per_timestep
        sim_trait_times_invented[[repl]]<-trait_times_invented_per_timestep
        sim_trait_times_learned[[repl]]<-trait_times_learned_per_timestep

        sim_times_SL[[repl]]<-times_SL
        sim_times_IN[[repl]]<-times_IN
        sim_times_SL_agents[[repl]]<-times_SL_agents
        sim_times_SL_success[[repl]]<-times_SL_success
        sim_times_IN_success[[repl]]<-times_IN_success
        sim_times_SL_success_agents[[repl]]<-times_SL_success_agents
        sim_times_IN_relative_success[[repl]]<-times_IN_relative_success
        sim_times_SL_relative_success[[repl]]<-times_SL_relative_success
        sim_times_SL_relative_success_agents[[repl]]<-times_SL_relative_success_agents

        sim_meanCumulativePayoffsAgents[[repl]]<-meanCumulativePayoffsAgents
        sim_meanCumulativePayoffsAgents_relative[[repl]]<-meanCumulativePayoffsAgents_relative

        sim_meanSLpayoffs[[repl]]<-meanSLpayoffs[!is.nan(meanSLpayoffs)]
        sim_meanSLpayoffs_raw[[repl]]<-meanSLpayoffs
        sim_meanSLpayoffsAgents[[repl]]<-meanSLpayoffsAgents[!is.nan(meanSLpayoffsAgents)]
        sim_meanSLpayoffsAgents_raw[[repl]]<-meanSLpayoffsAgents
        sim_relative_meanSLpayoffs[[repl]]<-relative_meanSLpayoffs[!is.nan(relative_meanSLpayoffs)]
        sim_relative_meanSLpayoffs_raw[[repl]]<-relative_meanSLpayoffs
        sim_relative_meanSLpayoffsAgents[[repl]]<-relative_meanSLpayoffsAgents[!is.nan(relative_meanSLpayoffsAgents)]
        sim_relative_meanSLpayoffsAgents_raw[[repl]]<-relative_meanSLpayoffsAgents

        df_overall_meanTraitsInSystem_replicates[simulation,repl]<-mean(meanTraitsInSystem)
        df_overall_meanTraitsAgents_replicates[simulation,repl]<-mean(meanTraitsAgents)
        df_overall_meanCumulativePayoffsAgents_replicates[simulation,repl]<-mean(meanCumulativePayoffsAgents)
        df_overall_meanCumulativePayoffsAgentsRelative_replicates[simulation,repl]<-mean(meanCumulativePayoffsAgents_relative)
        df_overall_meanSLrelativeSuccess_replicates[simulation,repl]<-mean(times_SL_relative_success)
        df_overall_meanSLrealtiveSuccessAgents_replicates[simulation,repl]<-mean(times_SL_relative_success_agents)
        df_overall_meanSLpayoffs_replicates[simulation,repl]<-mean(meanSLpayoffs[!is.nan(meanSLpayoffs)])
        df_overall_meanSLpayoffsAgents_replicates[simulation,repl]<-mean(meanSLpayoffsAgents[!is.nan(meanSLpayoffsAgents)])
        df_overall_meanSLpayoffsRelative_replicates[simulation,repl]<-mean(relative_meanSLpayoffs[!is.nan(relative_meanSLpayoffs)])
        df_overall_meanSLpayoffsAgentsRelative_replicates[simulation,repl]<-mean(relative_meanSLpayoffsAgents[!is.nan(relative_meanSLpayoffsAgents)])
        df_overall_meanSkillsInSystem_replicates[simulation,repl]<-mean(total_skills_in_system)
        df_overall_meanSkillsWithAgents_replicates[simulation,repl]<-mean(total_skills_with_agents)

        for (i in 1:timesteps){
            if (is.nan(meanSLpayoffs[i]) == TRUE){
                meanSLpayoffs[i]<-0
            }
            if (is.nan(meanSLpayoffsAgents[i]) == TRUE){
                meanSLpayoffsAgents[i]<-0
            }
            if (is.nan(relative_meanSLpayoffs[i]) == TRUE){
                relative_meanSLpayoffs[i]<-0
            }
            if (is.nan(relative_meanSLpayoffsAgents[i]) == TRUE){
                relative_meanSLpayoffsAgents[i]<-0
            }
        }
    
        sim_meanSLpayoffs_raw[[repl]]<-meanSLpayoffs
        sim_meanSLpayoffsAgents_raw[[repl]]<-meanSLpayoffsAgents
        sim_relative_meanSLpayoffs_raw[[repl]]<-relative_meanSLpayoffs
        sim_relative_meanSLpayoffsAgents_raw[[repl]]<-relative_meanSLpayoffsAgents

        sim_payoffs[[repl]]<-payoffs
        sim_SLpayoffs[[repl]]<-SLpayoffs
    }

    # # Descriptive statistics replicate simulations --------------------------------------------------
    overall_meanTraitsInSystem[simulation]<-mean(sapply(sim_meanTraitsInSystem, mean))
    df_sim1_meanTraitsInSystem_per_timestep<-data.frame(t(sapply(sim_meanTraitsInSystem,c)))
    write.csv(df_sim1_meanTraitsInSystem_per_timestep, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanTraitsInSystem_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanTraitsAgents[simulation]<-mean(sapply(sim_meanTraitsAgents, mean))
    df_sim1_meanTraitsAgents_per_timestep<-data.frame(t(sapply(sim_meanTraitsAgents,c)))
    write.csv(df_sim1_meanTraitsAgents_per_timestep, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanTraitsAgents_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanCumulativePayoffsAgents[simulation]<-mean(sapply(sim_meanCumulativePayoffsAgents, mean))
    df_sim1_meanCumulativePayoffsAgents<-data.frame(t(sapply(sim_meanCumulativePayoffsAgents,c)))
    write.csv(df_sim1_meanCumulativePayoffsAgents, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanCumulativePayoffsAgents_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanCumulativePayoffsAgents_relative[simulation]<-mean(sapply(sim_meanCumulativePayoffsAgents_relative, mean))
    df_sim1_meanCumulativePayoffsAgents_relative<-data.frame(t(sapply(sim_meanCumulativePayoffsAgents_relative,c)))
    write.csv(df_sim1_meanCumulativePayoffsAgents_relative, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanCumulativePayoffsAgentsRelative_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_SLrelativesuccess[simulation]<-mean(sapply(sim_times_SL_relative_success, mean))
    df_sim1_SLrelativesuccess<-data.frame(t(sapply(sim_times_SL_relative_success,c)))
    write.csv(df_sim1_SLrelativesuccess, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_SLrelativeSuccess_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_SLrelativesuccessagents[simulation]<-mean(sapply(sim_times_SL_relative_success_agents, mean))
    df_sim1_SLrelativesuccessagents<-data.frame(t(sapply(sim_times_SL_relative_success_agents,c)))
    write.csv(df_sim1_SLrelativesuccessagents, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_SLrelativeSuccessAgents_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanSLpayoffs[simulation]<-mean(sapply(sim_meanSLpayoffs, mean))
    df_sim1_meanSLpayoffs<-data.frame(t(sapply(sim_meanSLpayoffs_raw,c)))
    write.csv(df_sim1_meanSLpayoffs, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanSLpayoffs_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanSLpayoffs_relative[simulation]<-mean(sapply(sim_relative_meanSLpayoffs, mean))
    df_sim1_relative_meanSLpayoffs<-data.frame(t(sapply(sim_relative_meanSLpayoffs_raw,c)))
    write.csv(df_sim1_relative_meanSLpayoffs, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanSLpayoffsRelative_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanSLpayoffsAgents[simulation]<-mean(sapply(sim_meanSLpayoffsAgents, mean))
    df_sim1_meanSLpayoffsAgents<-data.frame(t(sapply(sim_meanSLpayoffsAgents_raw,c)))
    write.csv(df_sim1_meanSLpayoffsAgents, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanSLpayoffsAgents_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_meanSLpayoffsAgents_relative[simulation]<-mean(sapply(sim_relative_meanSLpayoffsAgents, mean))
    df_sim1_relative_meanSLpayoffsAgents<-data.frame(t(sapply(sim_relative_meanSLpayoffsAgents_raw,c)))
    write.csv(df_sim1_relative_meanSLpayoffsAgents, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_meanSLpayoffsAgentsRelative_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_mean_total_skills_in_system[simulation]<-mean(sapply(sim_skills_in_system, mean))
    df_sim1_skills_in_system<-data.frame(t(sapply(sim_skills_in_system,c)))
    write.csv(df_sim1_skills_in_system, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_totalSkillsInSystem_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    overall_mean_total_skills_with_agents[simulation]<-mean(sapply(sim_total_skills_with_agents, mean))
    df_sim1_total_skills_with_agents<-data.frame(t(sapply(sim_total_skills_with_agents,c)))
    write.csv(df_sim1_total_skills_with_agents, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_totalSkillsWithAgents_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

    df_sim1_repl1_skill_learned_per_timestep<-data.frame(sim_skill_learned_per_timestep[[1]])
    write.csv(df_sim1_repl1_skill_learned_per_timestep, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\",simulation,"_df_sim_",simulation,"_exampleSkillLearnedPerTimestep_",replicate_num_doc[simulation],"_",replicate_learningStrategy[simulation],"sp4_pd9"), row.names=FALSE)

}

df_overallMeans<-data.frame(overall_mean_total_skills_in_system,overall_mean_total_skills_with_agents,overall_meanCumulativePayoffsAgents, overall_meanCumulativePayoffsAgents_relative, overall_meanSLpayoffs, overall_meanSLpayoffs_relative, overall_meanSLpayoffsAgents, overall_meanSLpayoffsAgents_relative, overall_meanTraitsAgents, overall_meanTraitsInSystem, overall_SLrelativesuccess, overall_SLrelativesuccessagents)
write.csv(df_overallMeans, "C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overallMeans_sp4_pd9")

df_overall_meanTraitsInSystem_replicates<-as.data.frame(df_overall_meanTraitsInSystem_replicates)
write.csv(df_overall_meanTraitsInSystem_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanTraitsInSystem_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanTraitsAgents_replicates<-as.data.frame(df_overall_meanTraitsAgents_replicates)
write.csv(df_overall_meanTraitsAgents_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanTraitsAgents_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanCumulativePayoffsAgents_replicates<-as.data.frame(df_overall_meanCumulativePayoffsAgents_replicates)
write.csv(df_overall_meanCumulativePayoffsAgents_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanCumulativePayoffsAgents_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanCumulativePayoffsAgentsRelative_replicates<-as.data.frame(df_overall_meanCumulativePayoffsAgentsRelative_replicates)
write.csv(df_overall_meanCumulativePayoffsAgentsRelative_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanCumulativePayoffsAgentsRelative_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLrelativeSuccess_replicates<-as.data.frame(df_overall_meanSLrelativeSuccess_replicates)
write.csv(df_overall_meanSLrelativeSuccess_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLrelativeSuccess_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLrealtiveSuccessAgents_replicates<-as.data.frame(df_overall_meanSLrealtiveSuccessAgents_replicates)
write.csv(df_overall_meanSLrealtiveSuccessAgents_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLrealtiveSuccessAgents_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLpayoffs_replicates<-as.data.frame(df_overall_meanSLpayoffs_replicates)
write.csv(df_overall_meanSLpayoffs_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLpayoffs_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLpayoffsAgents_replicates<-as.data.frame(df_overall_meanSLpayoffsAgents_replicates)
write.csv(df_overall_meanSLpayoffsAgents_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLpayoffsAgents_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLpayoffsRelative_replicates<-as.data.frame(df_overall_meanSLpayoffsRelative_replicates)
write.csv(df_overall_meanSLpayoffsRelative_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLpayoffsRelative_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSLpayoffsAgentsRelative_replicates<-as.data.frame(df_overall_meanSLpayoffsAgentsRelative_replicates)
write.csv(df_overall_meanSLpayoffsAgentsRelative_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSLpayoffsAgentsRelative_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSkillsInSystem_replicates<-as.data.frame(df_overall_meanSkillsInSystem_replicates)
write.csv(df_overall_meanSkillsInSystem_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSkillsInSystem_replicates_sp4_pd9"), row.names=FALSE)

df_overall_meanSkillsWithAgents_replicates<-as.data.frame(df_overall_meanSkillsWithAgents_replicates)
write.csv(df_overall_meanSkillsWithAgents_replicates, paste0("C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\df_overall_meanSkillsWithAgents_replicates_sp4_pd9"), row.names=FALSE)