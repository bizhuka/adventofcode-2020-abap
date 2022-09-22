*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day24.
    DATA mt_input TYPE stringtab.
    METHODS setup.
    METHODS part1    FOR TESTING.
    METHODS part2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    mt_input = VALUE #(
        ( |sesenwnenenewseeswwswswwnenewsewsw| )
        ( |neeenesenwnwwswnenewnwwsewnenwseswesw| )
        ( |seswneswswsenwwnwse| )
        ( |nwnwneseeswswnenewneswwnewseswneseene| )
        ( |swweswneswnenwsewnwneneseenw| )
        ( |eesenwseswswnenwswnwnwsewwnwsene| )
        ( |sewnenenenesenwsewnenwwwse| )
        ( |wenwwweseeeweswwwnwwe| )
        ( |wsweesenenewnwwnwsenewsenwwsesesenwne| )
        ( |neeswseenwwswnwswswnw| )
        ( |nenwswwsewswnenenewsenwsenwnesesenew| )
        ( |enewnwewneswsewnwswenweswnenwsenwsw| )
        ( |sweneswneswneneenwnewenewwneswswnese| )
        ( |swwesenesewenwneswnwwneseswwne| )
        ( |enesenwswwswneneswsenwnewswseenwsese| )
        ( |wnwnesenesenenwwnenwsewesewsesesew| )
        ( |nenewswnwewswnenesenwnesewesw| )
        ( |eneswnwswnwsenenwnwnwwseeswneewsenese| )
        ( |neswnwewnwnwseenwseesewsenwsweewe| )
        ( |wseweeenwnesenwwwswnew| )
      ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( mt_input )
      exp = 10 ).
  ENDMETHOD.

  METHOD part2.
    TYPES: BEGIN OF ts_test,
             day    TYPE i,
             result TYPE i,
           END OF ts_test,
           tt_test TYPE STANDARD TABLE OF ts_test WITH DEFAULT KEY.
    DATA(lt_test) = VALUE tt_test(
       ( day = 1   result =  15   )
       ( day = 2   result =  12   )
       ( day = 3   result =  25   )
       ( day = 4   result =  14   )
       ( day = 5   result =  23   )
       ( day = 6   result =  28   )
       ( day = 7   result =  41   )
       ( day = 8   result =  37   )
       ( day = 9   result =  49   )
       ( day = 10  result =  37   )

       ( day = 20  result =  132  )
       ( day = 30  result =  259  )
       ( day = 40  result =  406  )
       ( day = 50  result =  566  )
       ( day = 60  result =  788  )
       ( day = 70  result =  1106 )
       ( day = 80  result =  1373 )
       ( day = 90  result =  1844 )
       ( day = 100 result =  2208 )
       ).

    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test>).
      cl_abap_unit_assert=>assert_equals(
        act = cut->part2( it_input = mt_input
                          iv_days  = <ls_test>-day )
        exp = <ls_test>-result ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
