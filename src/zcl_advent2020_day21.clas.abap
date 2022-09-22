class ZCL_ADVENT2020_DAY21 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  types:
    tt_ingredient TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .
  types:
    BEGIN OF ts_food,
        t_ingredients TYPE tt_ingredient,
        t_allergens   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      END OF ts_food .
  types:
    tt_food TYPE STANDARD TABLE OF ts_food WITH DEFAULT KEY .

  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type DECFLOAT34 .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_DANGEROUS) type STRING .
  PROTECTED SECTION.

private section.

  data MT_DANGEROUS_INGREDIENT type STRINGTAB .
  data MT_DANGEROUS_ALLERGEN type TT_INGREDIENT .

  methods _READ_FOOD
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RT_FOOD) type TT_FOOD .
  methods _GET_INTER_INGREDIENT
    importing
      !IT_2 type TT_INGREDIENT
    changing
      !CT_1 type TT_INGREDIENT .
  methods _DELETE_ALL
    importing
      !IV_ALLERGEN type STRING
      !IT_INGREDIENT type TT_INGREDIENT
    changing
      !CT_FOOD type TT_FOOD .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY21 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lt_food) = _read_food( it_input ).
    mt_dangerous_allergen   = VALUE #( ).
    mt_dangerous_ingredient = VALUE #( ).

    DATA(lv_changed) = abap_true.
    WHILE lv_changed = abap_true.
      lv_changed = abap_false.

      LOOP AT lt_food ASSIGNING FIELD-SYMBOL(<ls_food>).
        DATA(lv_index) = sy-tabix.
        CHECK lines( <ls_food>-t_allergens[] ) = 1.

        DATA(lv_allergen) = <ls_food>-t_allergens[ 1 ].
        IF lines( <ls_food>-t_ingredients[] ) = 1.
          lv_changed = abap_true.
          _delete_all( EXPORTING iv_allergen   = lv_allergen
                                 it_ingredient = <ls_food>-t_ingredients
                       CHANGING  ct_food       = lt_food ).

          CONTINUE.
        ENDIF.

        DATA(lt_set1) = <ls_food>-t_ingredients[].
        LOOP AT lt_food ASSIGNING FIELD-SYMBOL(<ls_food_cmp>).
          CHECK sy-tabix <> lv_index
            AND line_exists( <ls_food_cmp>-t_allergens[ table_line = lv_allergen ] ).

          _get_inter_ingredient( EXPORTING it_2 = <ls_food_cmp>-t_ingredients[]
                                 CHANGING  ct_1 = lt_set1 ).
          CHECK lines( lt_set1[] ) = 1.

          lv_changed = abap_true.
          _delete_all( EXPORTING iv_allergen   = lv_allergen
                                 it_ingredient = lt_set1
                       CHANGING  ct_food       = lt_food ).
        ENDLOOP.
      ENDLOOP.
    ENDWHILE.

    rv_count = REDUCE #( INIT s = 0
                         FOR <ls_row> IN lt_food
                         WHERE ( t_allergens IS INITIAL )
                         NEXT s = s + lines( <ls_row>-t_ingredients[] ) ).
  ENDMETHOD.


  METHOD part2.
    rv_dangerous = concat_lines_of( table = mt_dangerous_ingredient sep = `,` ).
  ENDMETHOD.


  METHOD _delete_all.
    ASSERT lines( it_ingredient[] ) = 1.
    DATA(lv_ingredient) = it_ingredient[ 1 ].

    LOOP AT ct_food ASSIGNING FIELD-SYMBOL(<ls_food>).
      DELETE <ls_food>-t_allergens[]   WHERE table_line = iv_allergen.
      DELETE <ls_food>-t_ingredients[] WHERE table_line = lv_ingredient.
    ENDLOOP.

    INSERT iv_allergen   INTO TABLE mt_dangerous_allergen[].
    READ TABLE mt_dangerous_allergen TRANSPORTING NO FIELDS
     WITH TABLE KEY table_line = iv_allergen.
    " Insert by index
    INSERT lv_ingredient INTO       mt_dangerous_ingredient[] INDEX sy-tabix.
  ENDMETHOD.


  METHOD _get_inter_ingredient.
    LOOP AT ct_1 INTO DATA(lv_ingredient).
      DATA(lv_tabix) = sy-tabix.

      READ TABLE it_2 TRANSPORTING NO FIELDS
       WITH TABLE KEY table_line = lv_ingredient.
      CHECK sy-subrc <> 0.

      DELETE ct_1 INDEX lv_tabix.
    ENDLOOP.
  ENDMETHOD.


  METHOD _read_food.
    LOOP AT it_input INTO DATA(lv_input).
      SPLIT lv_input AT ` (contains ` INTO DATA(lv_ingredients)
                                           DATA(lv_allergens).
      APPEND INITIAL LINE TO rt_food ASSIGNING FIELD-SYMBOL(<ls_food>).

      SPLIT lv_ingredients AT ` ` INTO TABLE DATA(lt_ingredients).
      INSERT LINES OF lt_ingredients INTO TABLE <ls_food>-t_ingredients[].

      REPLACE FIRST OCCURRENCE OF `)` IN lv_allergens WITH ``.
      SPLIT lv_allergens   AT `, ` INTO TABLE <ls_food>-t_allergens[].
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
