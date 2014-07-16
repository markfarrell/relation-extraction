#### Negated Clauses

Negated clauses are currently not handled by the software.

Here are a couple examples of negated simple declarative clauses:

The man did not sleep.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP
            (@VP (VBD did) (RB not))
            (VP (VB sleep))))
        (. .)))

The man might not have slept.

    (ROOT
      (S
        (@S
          (NP (DT The) (NN man))
          (VP
            (@VP (MD might) (RB not))
            (VP (VB have)
              (VP (VBD slept)))))
        (. .)))

## Recommendations

 * Visualize logical implications using a hypergraph structure (edges between edges).
