# PANDA-CS-3110-Team-Project

- Prajwal Reddy (pmr93)
- Alex Doboli (add82)
- Aditya Lingam (akl84)
- Dashiell Yamanishi (dly23)

Mirror of the team repository at github.coecis.cornell.edu/dly23/PANDA-CS-3110-Team-Project (Cornell login required).

╔════════════════════════════════════════╗
║        Welcome to Bamboo!              ║
╚════════════════════════════════════════╝
Syntax trees represent the ways to visualize sentence structures. 
The top of a tree, the root, typically contains a TP, which represents the sentence as a whole. The rule, TP -> DP VP, indicates that a TP node branches into two nodes, a DP and VP. The visualization is shown below:

  TP
  / \
 DP VP

A DP represents a noun phrase with a determiner (i.e, "the", "my", "his", "one", "four", etc.), while a VP represents a verb phrase. The structure of the sentence "The boy walks" is shown below:

       [TP] (7)        
          / \          
        /     \        
      /         \      
      |           \    
   [DP] (5)    [VP] (6)
     / \          |    
   /     \      V (2)  
D (4) [NP] (3)    |    
  |      |      walks  
 the   N (1)           
         |             
        boy            

It is important to note that we can not connect two lexical nodes together (i.e, N, V, D, P, etc.). For example, for a DP to be formed, we must first form an NP first. We cannot connect a D with an N as those are both lexical nodes. Right now, to connect nodes, use the rightmost id number as that represents the id number of the topmost node of the subtree.

Here are the lexical node representations:
N       - Noun
V       - Verb
D       - Determiner
P       - Preposition (not originally given)
Adj     - Adjective
