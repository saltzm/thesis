/*-------------------------------------------------------
 * match.cc
 * Definition of the match function
 ------------------------------------------------------*/

#include "argraph.h"
#include "match.h"
#include "state.h"
#include "error.h"


static bool match(int *pn, node_id c1[], node_id c2[], State *s);

static bool match(node_id c1[], node_id c2[], match_visitor vis, 
                 void *usr_data, State *s, int *pcount); 


/*-------------------------------------------------------------
 * bool match(s0, pn, c1, c2)
 * Finds a matching between two graph, if it exists, given the 
 * initial state of the matching process. 
 * Returns true a match has been found.
 * *pn is assigned the number of matched nodes, and
 * c1 and c2 will contain the ids of the corresponding nodes 
 * in the two graphs
 ------------------------------------------------------------*/
bool match(State *s0, int *pn, node_id c1[], node_id c2[])
  { 
    return match(pn,c1,c2,s0);
  }

/*------------------------------------------------------------
 * int match(s0, vis, usr_data)
 * Visits all the matches between two graphs, given the
 * initial state of the match.
 * Returns the number of visited matches.
 * Stops when there are no more matches, or the visitor vis
 * returns true.
 ----------------------------------------------------------*/
int match(State *s0, match_visitor vis, void *usr_data)
  { Graph *g1=s0->GetGraph1();
    Graph *g2=s0->GetGraph2();

    /* Choose a conservative dimension for the arrays */
    int n;
    if (g1->NodeCount()<g2->NodeCount())
      n=g2->NodeCount();
    else
      n=g1->NodeCount();

    node_id *c1=new node_id[n];
    node_id *c2=new node_id[n];

    if (!c1 || !c2)
      error("Out of memory");

    int count=0;
    match(c1, c2, vis, usr_data, s0, &count);

    delete[] c1;
    delete[] c2;
    return count;
  }



/*-------------------------------------------------------------
 * static bool match(pn, c1, c2, s)
 * Finds a matching between two graphs, if it exists, starting
 * from state s.
 * Returns true a match has been found.
 * *pn is assigned the numbero of matched nodes, and
 * c1 and c2 will contain the ids of the corresponding nodes 
 * in the two graphs.
 ------------------------------------------------------------*/
static bool match(int *pn, node_id c1[], node_id c2[], State *s)
  { if (s->IsGoal())
      { 
        *pn=s->CoreLen();
        s->GetCoreSet(c1, c2);
        return true;
      }

    if (s->IsDead())
      return false;

    node_id n1=NULL_NODE, n2=NULL_NODE;
    bool found=false;
    while (!found && s->NextPair(&n1, &n2, n1, n2))
      { if (s->IsFeasiblePair(n1, n2))
          { State *s1=s->Clone();
            s1->AddPair(n1, n2);
            found=match(pn, c1, c2, s1);
            s1->BackTrack();
            delete s1;
          }
      }
    return found;
  }




/*-------------------------------------------------------------
 * static bool match(c1, c2, vis, usr_data, pcount)
 * Visits all the matchings between two graphs,  starting
 * from state s.
 * Returns true if the caller must stop the visit.
 * Stops when there are no more matches, or the visitor vis
 * returns true.
 ------------------------------------------------------------*/
static bool match(node_id c1[], node_id c2[], 
                  match_visitor vis, void *usr_data, State *s, int *pcount) { 
    // IsGoal is a constant time check
    if (s->IsGoal()) {
        ++*pcount;
        int n=s->CoreLen();
        s->GetCoreSet(c1, c2);
        return vis(n, c1, c2, usr_data);
    }
    // Iterates through all rows after core_len.  if it finds a '1' it returns
    // false, otherwise, returns true 
    // O(n^2) vs mine which is O(1)
    if (s->IsDead())
      return false;

    node_id n1=NULL_NODE, n2=NULL_NODE;
    // NextPair iterates 1 to the right along a row, and searches for the next '1',
    // wrapping when it hits the end, if there isn't a '1' in the line, it returns false
    // which ends this search down the tree
    while (s->NextPair(&n1, &n2, n1, n2)) { 
        // IsFeasiblePair just makes sure neither of them are outside the matrix
        // and that M[n1][n2] == 1
        if (s->IsFeasiblePair(n1, n2)) { 
            State *s1=s->Clone();
            // adds the pair to the "core" set which is just two arrays representing
            // the mapping from g1 to g2 and calls "refine()" which i think is O(n^2)
            // refine:  
            //      for every untested future pairing
            //          if there is an edge from k to i, there must be an edge 
            //           from l to j and vice versa 
            //           (k is the previously added node from G1 and l its match in
            //           G2 that was added with AddPair)
            s1->AddPair(n1, n2);
            // Recursively call match
            if (match(c1, c2, vis, usr_data, s1, pcount)) { 
                s1->BackTrack();
                delete s1;
                return true;
            }
            else { 
                s1->BackTrack();
                delete s1;
	        }
        }
    }
    return false;
}




