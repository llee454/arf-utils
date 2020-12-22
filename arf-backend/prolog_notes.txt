  ++ Argument must be ground, i.e., the argument
     may not contain a variable anywhere.
  +  Argument must be fully instantiated to
     a term that satisfies the type. This is not
     necessarily ground, e.g., the term [_] is a
     list, although its only member is unbound.
  -  Argument is an output argument. Unless
     specified otherwise, output arguments need not
     to be unbound. For example, the goal findall(X,
     Goal, [T]) is good style and equivalent to
     findall(X, Goal, Xs), Xs = [T]46 Note that
     the determinism specification, e.g., ``det''
     only applies if this argument is unbound.
  -- Argument must be unbound. Typically used by
     predicates that create `something' and return
     a handle to the created object, such as open3
     which creates a stream.
  ?  Argument must be bound to a partial term
     of the indicated type. Note that a variable
     is a partial term for any type. Think of
     the argument as either input or output
     or both input and output. For example, in
     stream_property(S, reposition(Bool)), the
     reposition part of the term is input and the
     uninstantiated Bool is output.
  :  Argument is a meta-argument. Implies
     +. See chapter 6 for more information on
     module handling.
  @  Argument is not further
     instantiated. Typically used for type tests.
  !  Argument contains a mutable structure that
     may be modified using setarg3 or nb_setarg3.
