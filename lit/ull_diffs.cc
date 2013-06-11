static bool match(node_id c1[], node_id c2[], 
                          match_visitor vis, void *usr_data, State *s, int *pcount)
  { if (s->IsGoal())
            { ++*pcount;
                        int n=s->CoreLen();
                                s->GetCoreSet(c1, c2);
                                        return vis(n, c1, c2, usr_data);
                                              }

                if (s->IsDead())
                          return false;

                    node_id n1=NULL_NODE, n2=NULL_NODE;
                        while (s->NextPair(&n1, &n2, n1, n2))
                                  { if (s->IsFeasiblePair(n1, n2))
                                                { State *s1=s->Clone();
                                                                s1->AddPair(n1, n2);
                                                                            if (match(c1, c2, vis, usr_data, s1, pcount))
                                                                                              { s1->BackTrack();
                                                                                                                  delete s1;
                                                                                                                                  return true;
                                                                                                                                                }
                                                                                        else
                                                                                                          { s1->BackTrack();
                                                                                                                            delete s1;
                                                                                                                                          }
                                                                                                  }
                                                      }
                            return false;
                              }
