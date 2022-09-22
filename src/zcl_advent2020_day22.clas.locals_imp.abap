*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_input DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    DATA mt_input TYPE stringtab.
ENDCLASS.


CLASS lcl_input IMPLEMENTATION.
  METHOD constructor.
    mt_input = VALUE #(
         ( |Player 1:| )
         ( |3| )
         ( |42| )
         ( |4| )
         ( |25| )
         ( |14| )
         ( |36| )
         ( |32| )
         ( |18| )
         ( |33| )
         ( |10| )
         ( |35| )
         ( |50| )
         ( |16| )
         ( |31| )
         ( |34| )
         ( |46| )
         ( |9| )
         ( |6| )
         ( |41| )
         ( |7| )
         ( |15| )
         ( |45| )
         ( |30| )
         ( |27| )
         ( |49| )
         ( || )
         ( |Player 2:| )
         ( |8| )
         ( |11| )
         ( |47| )
         ( |21| )
         ( |17| )
         ( |39| )
         ( |29| )
         ( |43| )
         ( |23| )
         ( |28| )
         ( |13| )
         ( |22| )
         ( |5| )
         ( |20| )
         ( |44| )
         ( |38| )
         ( |26| )
         ( |37| )
         ( |2| )
         ( |24| )
         ( |48| )
         ( |12| )
         ( |19| )
         ( |1| )
         ( |40| )
    ).
  ENDMETHOD.
ENDCLASS.
