module TestData where
  -- 1. Simple leaf
  t1 = Leaf 1
  -- depth t1 == 1
  -- collapse t1 == [1]

  -- 2. Simple node with leaves
  t2 = Gnode [Leaf 'a', Leaf 'b', Leaf 'c']
  -- depth t2 == 2
  -- collapse t2 == ['a','b','c']

  -- 3. Deep linear nesting
  t3 =
    Gnode [
      Gnode [
        Gnode [
          Leaf 42
        ]
      ]
    ]
  -- depth t3 == 4
  -- collapse t3 == [42]

  -- 4. Even deeper chain
  t4 =
    Gnode [
      Gnode [
        Gnode [
          Gnode [
            Gnode [
              Leaf 99
            ]
          ]
        ]
      ]
    ]
  -- depth t4 == 6
  -- collapse t4 == [99]

  -- 5. Wide + empty node
  t5 =
    Gnode
      [ Leaf "x"
      , Leaf "y"
      , Leaf "z"
      , Gnode []       -- empty node
      , Leaf "w"
      ]
  -- depth t5 == 2        -- empty node doesn't add depth beyond the node itself
  -- collapse t5 == ["x","y","z","w"]

  -- 6. Completely empty node
  t6 = Gnode []
  -- depth t6 == 1
  -- collapse t6 == []

  -- 7. Mixed depth + width
  t7 =
    Gnode
      [ Leaf 1
      , Gnode
          [ Leaf 2
          , Gnode [Leaf 3, Leaf 4]
          , Leaf 5
          ]
      , Gnode
          [ Gnode [Leaf 6]
          , Leaf 7
          ]
      ]
  -- depth t7 == 4
  -- collapse t7 == [1,2,3,4,5,6,7]

  -- 8. Repeated patterns
  t8 =
    Gnode
      [ Gnode [Leaf 1, Leaf 2]
      , Gnode [Leaf 3, Leaf 4]
      , Gnode [Gnode [Leaf 5], Leaf 6]
      ]
  -- depth t8 == 4
  -- collapse t8 == [1,2,3,4,5,6]

  -- 9. Uneven alternating branches
  t9 =
    Gnode
      [ Leaf 10
      , Gnode [Leaf 20]
      , Gnode
          [ Gnode
              [ Leaf 30
              , Gnode [Leaf 40]
              ]
          ]
      , Leaf 50
      ]
  -- depth t9 == 5
  -- collapse t9 == [10,20,30,40,50]

  -- 10. Character "tree"
  t10 =
    Gnode
      [ Leaf '*'
      , Gnode
          [ Leaf '*'
          , Gnode [Leaf '*', Leaf '*']
          , Leaf '*'
          ]
      , Leaf '*'
      ]
  -- depth t10 == 4
  -- collapse t10 == ['*','*','*','*','*','*']

  -- 11. Spider-web shape
  t11 =
    Gnode
      [ Gnode [Leaf 1, Leaf 2]
      , Gnode
          [ Gnode [Leaf 3]
          , Gnode [Leaf 4, Leaf 5, Leaf 6]
          ]
      , Leaf 7
      , Gnode
          [ Gnode
              [ Gnode [Leaf 8]
              ]
          ]
      ]
  -- depth t11 == 5
  -- collapse t11 == [1,2,3,4,5,6,7,8]

