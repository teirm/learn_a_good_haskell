data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List []) = []
