*&---------------------------------------------------------------------*
*& Report ZCS_NEW_SYNTAX3
*&---------------------------------------------------------------------*
REPORT zcs_new_syntax3.

*&---------------------------------------------------------------------*
*& Inline Declaration
*& CASE Statements In OPEN SQL
*& Performing Calculations within SQL Statements
*& NEW Constructor Operator
*& Value Constructor Operator
*& FOR Iteration Expression
*& Chaining Operator
*& String Templates
*& Method Chaining
*& Avoiding TYPE MISMATCH Errors in ABAP
*& Filter Operator |WITOUT EXCEPT| WITH EXCEPT|
*& MAPPING |EXCEPT|
*& Table Comprehension
*& Table Comprehension : MULTIPLE FOR
*& Table Comprehension : Local Auxiliary Fields
*& Table Comprehension : Join
*& Table Comprehension : Multiple Rows
*& MESHES
*& IS INSTANCE OF
*& CASE TYPE OF
*& GROUP BY for Internal Tables
*& REDUCE
*& REDUCE |UNTIL|
*& FOR ... WHILE
*& FOR ... UNTIL
*& Predicative Method Calls
*& New Boolean Function
*& COND
*& SWITCH
*&---------------------------------------------------------------------*

*PARAMETERS: P_VBELN LIKE VBAK-VBELN.

* CASE Statements In OPEN SQL
CONSTANTS: lc_name1(5) TYPE c VALUE 'name1',
           lc_name2(5) TYPE c VALUE 'name2',
           lc_name3(5) TYPE c VALUE 'name3'.

SELECT vbeln,
       vbtyp,
       CASE
        WHEN auart = 'ZAMA' THEN @lc_name1
        WHEN auart = 'ZACR' THEN @lc_name2
        ELSE @lc_name3
       END AS ernam
       FROM vbak
       UP TO 10 ROWS
*       WHERE VBELN = @P_VBELN
       INTO TABLE @DATA(lt_vbak).

WRITE : 'CASE Statements In OPEN SQL'.
LOOP AT lt_vbak INTO DATA(ls_vbak).
  WRITE : / ls_vbak-vbeln,
            ls_vbak-vbtyp,
            ls_vbak-ernam.
ENDLOOP.

* Performing Calculations within SQL Statements
CONSTANTS: lc_carrid TYPE s_carr_id VALUE 'UA',
           lc_connid TYPE s_conn_id VALUE '941'.

SELECT carrid,
       connid,
       price,
       seatsocc_b,
       seatsocc_f,
       ( ( seatsocc_b + seatsocc_f ) ) * price AS paymentsum
       FROM sflight
       WHERE carrid = @lc_carrid
       AND connid = @lc_connid
       INTO TABLE @DATA(lt_sflight).

SKIP.

WRITE :/ 'Performing Calculations within SQL Statements'.
LOOP AT lt_sflight INTO DATA(ls_sflight).
  WRITE : / ls_sflight-carrid,
            ls_sflight-connid,
            ls_sflight-price,
            ls_sflight-seatsocc_b,
            ls_sflight-seatsocc_f,
            ls_sflight-paymentsum.
ENDLOOP.

SKIP.

*NEW Constructor Operator
WRITE : / 'NEW Constructor Operator'.
CLASS zcl_student DEFINITION.
  PUBLIC SECTION.
    METHODS : constructor.
ENDCLASS.

DATA(lo_cl_student) = NEW zcl_student( ).

SKIP.

*Value Constructor Operator
WRITE : / 'Value Constructor Operator'.
* create internal table using VALUE operator
*TYPES t_itab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
TYPES : BEGIN OF ty_itab,
          i TYPE i,
        END OF ty_itab,
        t_itab TYPE STANDARD TABLE OF ty_itab WITH DEFAULT KEY.

DATA(itab) =  VALUE t_itab( ( i = 10 )
                            ( i = 20 )
                            ( i = 30 ) ) .

WRITE : / 'ITAB filled with value operator'.
WRITE : / '      COL I'.
LOOP AT itab INTO DATA(wa_itab).
  WRITE : / wa_itab-i.
ENDLOOP.
SKIP.

*FOR Iteration Expression
WRITE : / 'FOR Iteration Expression'.

TYPES: BEGIN OF ty_ship,
         tknum TYPE tknum,
         name  TYPE ernam,
         city  TYPE ort01,
         route TYPE route,
       END OF ty_ship.

TYPES: ty_ships TYPE SORTED TABLE OF ty_ship WITH UNIQUE KEY tknum.
TYPES: ty_citys TYPE STANDARD TABLE OF ort01 WITH EMPTY KEY.
DATA: gt_ships TYPE ty_ships.
*DATA: GT_CITYS TYPE TY_CITYS,
*      GS_SHIP  TYPE TY_SHIP,
*      GS_CITY  TYPE ORT01.
*
*LOOP AT GT_SHIPS INTO GS_SHIP.
*  GS_CITY = GS_SHIP-CITY.
*  APPEND GS_CITY TO GT_CITYS.
*ENDLOOP.

DATA(gt_citys1) = VALUE ty_citys( FOR ls_ship IN gt_ships ( ls_ship-city ) ).

*LOOP AT GT_SHIPS INTO GS_SHIP WHERE ROUTE = 'R0001'.
*  GS_CITY = GS_SHIP-CITY.
*  APPEND GS_CITY TO GT_CITYS.
*ENDLOOP.

DATA(gt_citys2) = VALUE ty_citys( FOR ls_ship IN gt_ships
                       WHERE ( route = 'R0001' ) ( ls_ship-city ) ).
SKIP.

DATA(lt_knbk_diff) = VALUE knbk_t
                                ( FOR ls_knbk IN COND #( WHEN lines( lt_knbk1 ) >= lines( lt_knbk2 ) THEN lt_knbk1 ELSE lt_knbk2 )    
                                ( LINES OF COND #( 
                                                  WHEN lines( lt_knbk1 ) >= lines( lt_knbk2 ) AND 
                                                    NOT line_exists( lt_knbk2[ table_line = ls_knbk ] ) 
                                                    THEN VALUE #( ( ls_knbk ) )

                                                  WHEN lines( lt_knbk2 ) > lines( lt_knbk1 ) AND 
                                                    NOT line_exists( lt_knbk1[ table_line = ls_knbk ] ) 
                                                    THEN VALUE #( ( ls_knbk ) ) 
                                                ) 
                                )
                                ).

*Chaining Operator
WRITE : / 'Chaining Operator'.
DATA: v_var1 TYPE char30,
      v_var2 TYPE char30,
      v_var3 TYPE char30.

DATA : lv_result TYPE string.

v_var1 = 'Building'.
v_var2 = 'A'.
v_var3 = 'String'.

lv_result = v_var1 && v_var2 && v_var3.
WRITE : / 'Using &&', lv_result.

SKIP.
*String Templates
WRITE : / 'String Templates'.
DATA: character_string TYPE string.
character_string = |This is a literal text.|.

WRITE :/ character_string.

SKIP.
*Method Chaining
WRITE : / 'Method Chaining'.
CLASS zcx_exception DEFINITION INHERITING FROM cx_static_check.

ENDCLASS.

CLASS zcl_my_screen_message DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS : display IMPORTING im_error TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

DATA(lo_exception) = NEW zcx_exception( ).

*TRY.
*  CATCH ZCX_EXCEPTION INTO LO_EXCEPTION.
*    DATA(LV_ERROR_TXT) = LO_EXCEPTION->GET_LONGTEXT( ).
*    ZCL_MY_SCREEN_MESSAGE=>DISPLAY( IM_ERROR = LV_ERROR_TXT ).
*ENDTRY.

TRY.
  CATCH zcx_exception INTO lo_exception.
    zcl_my_screen_message=>display( im_error = lo_exception->get_longtext( ) ).
ENDTRY.

SKIP.

*Avoiding TYPE MISMATCH Errors in ABAP
WRITE : / 'Avoiding TYPE MISMATCH Errors in ABAP'.

DATA: ld_po_number       TYPE ebeln.

CLASS zcl_purorder DEFINITION.
  PUBLIC SECTION.
    METHODS : get_items IMPORTING id_po_number       TYPE ebeln
                        EXPORTING ed_number_of_items TYPE i.
ENDCLASS.

DATA(lo_purorder) = NEW zcl_purorder( ).

*LO_PURORDER->GET_ITEMS( EXPORTING ID_PO_NUMBER = LD_PO_NUMBER
*                        IMPORTING ED_NUMBER_OF_ITEMS = LD_NUMBER_OF_ITEMS ).

lo_purorder->get_items( EXPORTING id_po_number = ld_po_number
                        IMPORTING ed_number_of_items = DATA(ld_number_of_items2) ).

CLASS zcl_mercedes_benz DEFINITION.
  PUBLIC SECTION.
    METHODS : constructor.
ENDCLASS.

CLASS zcl_car_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS : build_new_car RETURNING VALUE(eo_mercedes_benz) TYPE REF TO zcl_mercedes_benz.
ENDCLASS.

*DATA: LO_VEHICLE TYPE REF TO ZCL_MERCEDES_BENZ.
*LO_VEHICLE = ZCL_CAR_FACTORY=>BUILD_NEW_CAR( ).

DATA(lo_vehicle2) = zcl_car_factory=>build_new_car( ).

SKIP.

*Filter Operator
WRITE : / 'Filter Operator'.

*DATA LT_ALL_MATERIAL TYPE STANDARD TABLE OF MARA
*                     WITH NON-UNIQUE SORTED KEY MATNR
*                     COMPONENTS MATNR.
*SELECT *
*       FROM MARA
*       INTO TABLE @DATA(LT_ALL_MATERIALS)
*       UP TO 10000 ROWS.
*
*DATA(LT_MATERIALS_FERT) = FILTER #( LT_ALL_MATERIALS USING KEY MATNR
*                                                     WHERE MATNR = 'A001989210399' ).

SELECT *
       FROM scarr
       INTO TABLE @DATA(carriers).

DATA filter TYPE SORTED TABLE OF scarr-carrid
                 WITH UNIQUE KEY table_line.
filter = VALUE #( ( 'AA ' ) ( 'LH ' ) ( 'UA ' ) ).

WRITE : / 'SCARR'.
WRITE : / 'CAR CARRNAME           CURRC URL'.
LOOP AT carriers INTO DATA(ls_carriers).
  WRITE : / ls_carriers-carrid,
            ls_carriers-carrname,
            ls_carriers-currcode,
            ls_carriers-url.
ENDLOOP.

*WITHOUT EXCEPT
WRITE : / 'WITHOUT EXCEPT'.
*cl_demo_output=>display( FILTER #(
*   carriers IN filter WHERE carrid = table_line ) ).

DATA(lt_scarr) = FILTER #( carriers IN filter WHERE carrid = table_line ).
WRITE : / 'LT_SCARR TABLE FILLED FILTER WITHOUT EXCEPT'.
WRITE : / 'CAR CARRNAME           CURRC URL'.
LOOP AT lt_scarr INTO DATA(ls_scarr).
  WRITE : / ls_scarr-carrid,
            ls_scarr-carrname,
            ls_scarr-currcode,
            ls_scarr-url.
ENDLOOP.
*WITH EXCEPT
WRITE : / 'WITH EXCEPT'.
*cl_demo_output=>display( FILTER #(
*   carriers EXCEPT IN filter WHERE carrid = table_line ) ).

DATA(lt_scarr2) = FILTER #( carriers EXCEPT IN filter WHERE carrid = table_line ).
WRITE : / 'LT_SCARR2 TABLE FILLED FILTER WITHOUT EXCEPT'.
WRITE : / 'CAR CARRNAME           CURRC URL'.
LOOP AT lt_scarr2 INTO DATA(ls_scarr2).
  WRITE : / ls_scarr2-carrid,
            ls_scarr2-carrname,
            ls_scarr2-currcode,
            ls_scarr2-url.
ENDLOOP.
SKIP.

*MAPPING
WRITE : / 'Mapping'.

DATA: BEGIN OF struct1,
        mcomp1 TYPE i VALUE 1,
        mcomp2 TYPE i VALUE 2,
        BEGIN OF substruc,
          subcomp1 TYPE i VALUE 1,
          subcomp2 TYPE i VALUE 2,
          subcomp3 TYPE i VALUE 3,
        END OF substruc,
      END OF struct1.

DATA: BEGIN OF struct2,
        comp2 TYPE i,
        comp1 TYPE i,
        BEGIN OF substruc,
          comp3 TYPE i,
          comp2 TYPE i,
          comp1 TYPE i,
        END OF substruc,
      END OF struct2.

struct2 =
  CORRESPONDING #(
    struct1 MAPPING comp1    = mcomp1
                    comp2    = mcomp2
                  ( substruc = substruc MAPPING comp1 = subcomp1
                                                comp2 = subcomp2
                                                comp3 = subcomp3 ) ) .
WRITE : / 'STRUCT1'.
WRITE : / 'MCOMP1 MCOMP2 SUBCOMP1 SUBCOMP2 SUBCOMP3'.
WRITE : / struct1-mcomp1,
          struct1-mcomp2,
          struct1-substruc-subcomp1,
          struct1-substruc-subcomp2,
          struct1-substruc-subcomp3.

WRITE : / 'STRUCT2 COPIED USING CORRESPONDIONG MAPPING OPERATORS'.
WRITE : / 'COMP2 COMP1 COMP3 COMP2 COMP1'.
WRITE : / struct2-comp2,
          struct2-comp1,
          struct2-substruc-comp3,
          struct2-substruc-comp2,
          struct2-substruc-comp1.

*MAPPING EXCEPT EXAMPLE
DATA:
  BEGIN OF struct3,
    col1 TYPE string VALUE `COL1`,
    col2 TYPE string VALUE `COL2`,
    col3 TYPE string VALUE `COL3`,
    col4 TYPE string VALUE `COL4`,
  END OF struct3,
  BEGIN OF struct4,
    col4 TYPE string,
    col3 TYPE string,
    col2 TYPE string,
    col1 TYPE string,
  END OF struct4,
  struct5 LIKE struct4.

WRITE : / 'STRUCT3'.
WRITE : / 'COL1 COL2 COL3 COL4'.
WRITE : / struct3-col1,
          struct3-col2,
          struct3-col3,
          struct3-col4.

struct4 = CORRESPONDING #( struct3 ).
WRITE : / 'STRUCT4 COPIED FROM STRUCT3'.
WRITE : / 'COL1 COL2 COL3 COL4'.
WRITE : / struct4-col1,
          struct4-col2,
          struct4-col3,
          struct4-col4.

*Here mapping the struct5 with 3 but without col2 and col3
struct5 = CORRESPONDING #( struct3 EXCEPT col2 col3 ).
WRITE : / 'STRUCT5 COPIED FROM STRUCT3 but without col2 and col3'.
WRITE : / 'COL1 COL2 COL3 COL4'.
WRITE : / struct5-col1,
          struct5-col2,
          struct5-col3,
          struct5-col4.

*cl_demo_output=>new(
*  )->write(   struct3
*  )->write(   struct4
*  )->display( struct5 ).

*Table Comprehension
WRITE : / 'Table Comprehension'.

* MULTIPLE FOR
TYPES:
  BEGIN OF line,
    col1 TYPE i,
    col2 TYPE i,
  END OF line,
  BEGIN OF line1,
    col1 TYPE i,
    col2 TYPE STANDARD TABLE OF line WITH EMPTY KEY,
  END OF line1,
  itab1 TYPE STANDARD TABLE OF line1 WITH EMPTY KEY,
  BEGIN OF line2,
    col1 TYPE i,
    col2 TYPE i,
    col3 TYPE i,
  END OF line2,
  itab2 TYPE STANDARD TABLE OF line2 WITH EMPTY KEY.

DATA(out) = cl_demo_output=>new( ).

DATA(itab1) = VALUE itab1(
  ( col1 = 1 col2 = VALUE line1-col2( ( col1 = 111 col2 = 112 )
                                      ( col1 = 121 col2 = 122 ) ) )
  ( col1 = 2 col2 = VALUE line1-col2( ( col1 = 211 col2 = 212 )
                                      ( col1 = 221 col2 = 222 ) ) )
  ( col1 = 3 col2 = VALUE line1-col2( ( col1 = 311 col2 = 312 )
                                      ( col1 = 321 col2 = 322 ) ) )
                         ).
WRITE : / 'ITAB1 TABLE'.
WRITE : / '       COL1       COL2        COL3'.
LOOP AT itab1 INTO DATA(line1).
*  out->write( name = |ITAB1, Line { sy-tabix }, COL2|
*              data = line1-col2 ).
  WRITE : / |ITAB1, Line { sy-tabix }, COL2|.
  WRITE : / line1-col1.
  LOOP AT line1-col2 INTO DATA(line2).
    WRITE : line2-col1 , line2-col2.
  ENDLOOP.
  SKIP.
ENDLOOP.

DATA(itab2) = VALUE itab2(
  FOR wa1 IN itab1
  FOR wa2 IN wa1-col2
    ( col1 = wa1-col1
      col2 = wa2-col1
      col3 = wa2-col2 ) ).

WRITE : / 'ITAB2 TABLE FILLED WITH NESTED FOR'.
WRITE : / '       COL1       COL2        COL3'.
LOOP AT itab2 INTO DATA(ls_itab2).
  WRITE : / ls_itab2-col1,
            ls_itab2-col2,
            ls_itab2-col3.
ENDLOOP.
*out->write( itab2 ).
*out->display( ).

*Table Comprehensions, Local Auxiliary Fields
WRITE : / 'Table Comprehensions, Local Auxiliary Fields'.

TYPES:
  array TYPE STANDARD TABLE OF i WITH EMPTY KEY,
  BEGIN OF line3,
    col1 TYPE i,
    col2 TYPE i,
    col3 TYPE i,
  END OF line3,
  itab TYPE STANDARD TABLE OF line3 WITH EMPTY KEY.

CONSTANTS factor TYPE i VALUE 1000.

DATA(array) = VALUE array(
  ( 3 ) ( 5 ) ( 7 ) ( 9 ) ).

DATA(itab3) = VALUE itab(
  FOR x IN array INDEX INTO idx
     LET off = factor * idx IN
    ( col1 = x col2 = x * x col3 = x + off ) ).

WRITE : / 'ITAB3 TABLE FILLED WITH FOR, AUXILIARY FIELDS'.
WRITE : / '       COL1       COL2        COL3'.
LOOP AT itab3 INTO DATA(ls_itab3).
  WRITE : / ls_itab3-col1,
            ls_itab3-col2,
            ls_itab3-col3.
ENDLOOP.

*cl_demo_output=>display( itab3 ).

*Table Comprehensions, Join
WRITE : / 'Table Comprehensions, Join'.
*Nested loops
TYPES:
  BEGIN OF line4,
    key  TYPE c LENGTH 1,
    col1 TYPE i,
    col2 TYPE i,
  END OF line4,
  itab4 TYPE STANDARD TABLE OF line4 WITH EMPTY KEY,
  BEGIN OF line5,
    key  TYPE c LENGTH 1,
    col1 TYPE i,
    col2 TYPE i,
  END OF line5,
  itab5 TYPE STANDARD TABLE OF line5 WITH EMPTY KEY,
  BEGIN OF line6,
    key   TYPE c LENGTH 1,
    col11 TYPE i,
    col12 TYPE i,
    col21 TYPE i,
    col22 TYPE i,
  END OF line6,
  itab6 TYPE STANDARD TABLE OF line6 WITH EMPTY KEY.

out = cl_demo_output=>new( ).

DATA(itab4) = VALUE itab4(
  ( key = 'a' col1 = 11 col2 = 12 )
  ( key = 'a' col1 = 11 col2 = 13 )
  ( key = 'b' col1 = 21 col2 = 22 )
  ( key = 'c' col1 = 31 col2 = 32 ) ).
*out->write( itab4 ).
WRITE : / 'ITAB4 TABLE'.
WRITE : / 'KEY       COL1       COL2'.
LOOP AT itab4 INTO DATA(ls_itab4).
  WRITE : / ls_itab4-key,
            ls_itab4-col1,
            ls_itab4-col2.
ENDLOOP.

DATA(itab5) = VALUE itab5(
  ( key = 'a' col1 = 13 col2 = 14 )
  ( key = 'a' col1 = 13 col2 = 15 )
  ( key = 'b' col1 = 23 col2 = 24 )
  ( key = 'c' col1 = 33 col2 = 34 ) ).
*out->write( itab5 ).
WRITE : / 'ITAB5 TABLE'.
WRITE : / 'KEY       COL1       COL2'.
LOOP AT itab5 INTO DATA(ls_itab5).
  WRITE : / ls_itab5-key,
            ls_itab5-col1,
            ls_itab5-col2.
ENDLOOP.

DATA(itab6) = VALUE itab6(
  FOR wa IN itab4
    ( key   = wa-key
      col11 = wa-col1
      col12 = wa-col2
      col21 = itab5[ key = wa-key ]-col1
      col22 = itab5[ key = wa-key ]-col2 ) ).
*out->write( itab6 ).
WRITE : / 'ITAB6 TABLE CREATED WITH FOR WITTH ARITHEMTIC OPERATORS'.
WRITE : / 'KEY      COL11      COL12      COL21      COL22'.
LOOP AT itab6 INTO DATA(ls_itab6).
  WRITE : / ls_itab6-key,
            ls_itab6-col11,
            ls_itab6-col12,
            ls_itab6-col21,
            ls_itab6-col22.
ENDLOOP.

DATA(itab7) = VALUE itab6(
  FOR wa3 IN itab4 INDEX INTO idx
  FOR wa4 IN itab5 WHERE ( key = wa3-key )
    ( key   = wa3-key
      col11 = wa3-col1
      col12 = wa3-col2
      col21 = wa4-col1
      col22 = wa4-col2 ) ).
*out->write( itab7 ).
*out->display( ).
WRITE : / 'ITAB7 TABLE CREATED WITH ITAB4 AND 5 USING NESTED FOR SIMILAR TO JOIN'.
WRITE : / 'KEY      COL11      COL12      COL21      COL22'.
LOOP AT itab7 INTO DATA(ls_itab7).
  WRITE : / ls_itab7-key,
            ls_itab7-col11,
            ls_itab7-col12,
            ls_itab7-col21,
            ls_itab7-col22.
ENDLOOP.

*Table Comprehensions, Multiple Rows
WRITE : / 'Table Comprehensions, Multiple Rows'.

TYPES:
  BEGIN OF line7,
    col1 TYPE i,
    col2 TYPE i,
    col3 TYPE i,
  END OF line7,
  itab8 TYPE STANDARD TABLE OF line7 WITH EMPTY KEY,
  itab9 TYPE STANDARD TABLE OF i     WITH EMPTY KEY.

out = cl_demo_output=>new( ).

DATA(itab8) = VALUE itab8(
  ( col1 = 11 col2 = 12 col3 = 13 )
  ( col1 = 21 col2 = 22 col3 = 23 )
  ( col1 = 31 col2 = 32 col3 = 33 ) ).
out->write( itab8 ).
WRITE : / 'ITAB8 TABLE'.
WRITE : / '       COL1       COL2        COL3'.
LOOP AT itab8 INTO DATA(ls_itab8).
  WRITE : / ls_itab8-col1,
            ls_itab8-col2,
            ls_itab8-col3.
ENDLOOP.

DATA(itab9) = VALUE t_itab(
  FOR wa5 IN itab8
    ( i = wa5-col1 )
    ( i = wa5-col2 )
    ( i = wa5-col3 ) ).
*out->write( itab9 ).
*out->display( ).
WRITE : / 'ITAB9 TABLE CREATED USING MULTIPLE ROWS IN FOR '.
WRITE : / '       COL1       COL2        COL3'.
LOOP AT itab9 INTO DATA(ls_itab9).
  WRITE : / ls_itab9-i.
ENDLOOP.

*MESHES
WRITE :/ 'MESHES'.
TYPES:
  t_scarr   TYPE SORTED TABLE OF scarr
            WITH UNIQUE KEY carrid,
  t_spfli   TYPE SORTED TABLE OF spfli
            WITH UNIQUE KEY carrid connid,
  t_sflight TYPE SORTED TABLE OF sflight
            WITH UNIQUE KEY carrid connid fldate.

TYPES:
  BEGIN OF MESH t_flights,
    scarr   TYPE t_scarr
      ASSOCIATION to_spfli TO spfli
               ON carrid = carrid USING KEY primary_key,
    spfli   TYPE t_spfli
      ASSOCIATION to_sflight TO sflight
               ON carrid = carrid AND
                  connid = connid USING KEY primary_key,
    sflight TYPE t_sflight,
  END OF MESH t_flights.

DATA:
  flights TYPE t_flights.

*SELECT * FROM SCARR INTO TABLE @DATA(LT_SCARR).
*SELECT * FROM SPFLI INTO TABLE @DATA(LT_SPFLI).
*SELECT * FROM SFLIGHT INTO TABLE @DATA(LT_SFLIGHT2).
*
**Error : Unable to interpret "FOR". Possible causes of error include incorrect spellings or comma errors.
*FLIGHTS = VALUE T_FLIGHTS(
*            FOR WA_SCARR IN LT_SCARR
*            FOR WA_SPFLI IN LT_SPFLI FROM LINE_INDEX( LT_SPFLI[ CARRID = WA_SCARR-CARRID ] )
*                                     WHERE ( CARRID = WA_SCARR-CARRID )
*            FOR WA_SFLIGHT IN LT_SFLIGHT2 FROM LINE_INDEX( LT_SFLIGHT2[ CARRID = WA_SPFLI-CARRID
*                                                                        CONNID = WA_SPFLI-CONNID ] )
*                                          WHERE ( CARRID = WA_SPFLI-CARRID AND CONNID = WA_SPFLI-CONNID )
*            ( CARRID    = WA_SCARR-CARRID
*              CARRNAME  = WA_SCARR-CARRNAME
*              CURRCODE  = WA_SCARR-CURRCODE
*              URL       = WA_SCARR-URL
*              CONNID    = WA_SPFLI-CONNID
*              COUNTRYFR = WA_SPFLI-COUNTRYFR
*              CITYFROM  = WA_SPFLI-CITYFROM
*              AIRPFROM  = WA_SPFLI-AIRPFROM
*              COUNTRYTO = WA_SPFLI-COUNTRYTO
*              CITYTO    = WA_SPFLI-CITYTO
*              AIRPTO    = WA_SPFLI-AIRPTO
*              FLTIME    = WA_SPFLI-FLTIME
*              DEPTIME   = WA_SPFLI-DEPTIME
*              ARRTIME   = WA_SPFLI-ARRTIME
*              DISTANCE  = WA_SPFLI-DISTANCE
*              DISTID    = WA_SPFLI-DISTID
*              FLTYPE    = WA_SPFLI-FLTYPE
*              PERIOD    = WA_SPFLI-PERIOD
*              FLDATE    = WA_SFLIGHT-FLDATE
*              PRICE     = WA_SFLIGHT-PRICE
*              CURRENCY   = WA_SFLIGHT-CURRENCY
*              PLANETYPE  = WA_SFLIGHT-PLANETYPE
*              SEATSMAX   = WA_SFLIGHT-SEATSMAX
*              SEATSOCC   = WA_SFLIGHT-SEATSOCC
*              PAYMENTSUM = WA_SFLIGHT-PAYMENTSUM
*              SEATSMAX_B = WA_SFLIGHT-SEATSMAX_B
*              SEATSOCC_B = WA_SFLIGHT-SEATSOCC_B
*              SEATSMAX_F = WA_SFLIGHT-SEATSMAX_F
*              SEATSOCC_F = WA_SFLIGHT-SEATSOCC_F
*            )
*          ).

*DATA(ROOT) = FLIGHTS-SCARR[ CARRNAME = 'United Airlines' ].
*
*LOOP AT
*  FLIGHTS-SCARR\TO_SPFLI[ ROOT ]\TO_SFLIGHT[ ]
*    INTO DATA(WA6).
*ENDLOOP.

SKIP.
*IS INSTANCE OF
WRITE : / 'IS INSTANCE OF'.

DATA(lo_instance_of) = cl_abap_typedescr=>describe_by_data( p_data = '' ).

IF lo_instance_of IS INSTANCE OF cl_abap_elemdescr.
  DATA(elemdescr) = CAST cl_abap_elemdescr( lo_instance_of ).
  WRITE : / 'CL_ABAP_ELEMDESCR'.
ELSEIF lo_instance_of IS INSTANCE OF cl_abap_structdescr.
  DATA(structdescr) = CAST cl_abap_structdescr( lo_instance_of ).
  WRITE : / 'CL_ABAP_STRUCTDESCR'.
ELSEIF lo_instance_of IS INSTANCE OF cl_abap_tabledescr.
  DATA(tabledescr) = CAST cl_abap_tabledescr( lo_instance_of ).
  WRITE : / 'CL_ABAP_TABLEDESCR'.
ELSE.
  WRITE : / 'OTHER INSTANCE'.
ENDIF.

SKIP.
*CASE TYPE OF
WRITE : / 'CASE TYPE OF'.

DATA(lo_instance_of2) = cl_abap_typedescr=>describe_by_data( p_data = '' ).

CASE TYPE OF lo_instance_of2.
  WHEN TYPE cl_abap_elemdescr INTO DATA(elemdescr2).
    WRITE : / 'CL_ABAP_ELEMDESCR'.
  WHEN TYPE cl_abap_structdescr INTO DATA(structdescr2).
    WRITE : / 'CL_ABAP_STRUCTDESCR'.
  WHEN TYPE cl_abap_tabledescr INTO DATA(tabledescr2).
    WRITE : / 'CL_ABAP_TABLEDESCR'.
  WHEN OTHERS.
    WRITE : / 'OTHER INSTANCE'.
ENDCASE.

SKIP.
*GROUP BY for Internal Tables
WRITE : / 'GROUP BY for Internal Tables'.
DATA lt_flights2 TYPE TABLE OF spfli WITH EMPTY KEY.

SELECT * FROM  spfli
         INTO TABLE @lt_flights2.

DATA members LIKE lt_flights2.
LOOP AT lt_flights2 INTO DATA(wa_flight)
     GROUP BY ( carrier = wa_flight-carrid cityfr = wa_flight-cityfrom )
              ASCENDING
              ASSIGNING FIELD-SYMBOL(<group>).
  CLEAR members.
  LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<flight>).
    WRITE : / <flight>-carrid,
              <flight>-connid,
              <flight>-countryfr,
              <flight>-cityfrom,
              <flight>-airpfrom,
              <flight>-countryto,
              <flight>-cityto.
    members = VALUE #( BASE members ( <flight> ) ).
  ENDLOOP.
  SKIP.
*  cl_demo_output=>write( members ).
ENDLOOP.
*cl_demo_output=>display( ).

skip.
*REDUCE
WRITE : / 'Reduce'.
TYPES:
  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    netwr TYPE ekpo-netwr,
  END OF ty_ekpo,
  ytt_ekpo TYPE SORTED TABLE OF ty_ekpo
          WITH UNIQUE KEY ebeln ebelp,

  BEGIN OF ty_komv,
    knumv TYPE komv-knumv,
    kposn TYPE komv-kposn,
    kschl TYPE komv-kschl,
    kwert TYPE komv-kwert,
  END OF ty_komv,
  ytt_komv TYPE SORTED TABLE OF ty_komv
          WITH NON-UNIQUE KEY knumv kposn kschl
          WITH NON-UNIQUE SORTED KEY key_kposn COMPONENTS kposn kschl.

DATA it_ekpo  TYPE ytt_ekpo.
DATA it_komv  TYPE ytt_komv.

it_ekpo =
  VALUE #(
    ( ebeln = '0040000000' ebelp = '10'	 )
    ( ebeln = '0040000000' ebelp = '20'	 )
    ( ebeln = '0040000000' ebelp = '30'	 )
          ).

WRITE : / 'IT_EKPO TABLE'.
LOOP AT it_ekpo INTO DATA(ls_ekpo).
  WRITE : ls_ekpo-ebeln,
          ls_ekpo-ebelp,
          ls_ekpo-netwr.
ENDLOOP.

it_komv =
  VALUE #(
    ( knumv = '0000000001' kposn = '10'	kschl = 'RA01' kwert = '10.00'  )
    ( knumv = '0000000001' kposn = '10'	kschl = 'PBXX' kwert = '350.00' )
    ( knumv = '0000000001' kposn = '20'	kschl = 'RA01' kwert = '2.00'   )
    ( knumv = '0000000001' kposn = '20'	kschl = 'RA01' kwert = '3.50'   )
    ( knumv = '0000000001' kposn = '20'	kschl = 'PBXX' kwert = '400.00' )
    ( knumv = '0000000001' kposn = '10'	kschl = 'RA01' kwert = '5.00'   )
    ( knumv = '0000000001' kposn = '10'	kschl = 'PBXX' kwert = '200.00' )
          ).

WRITE : / 'IT_KOMV TABLE'.
LOOP AT it_komv INTO DATA(ls_komv).
  WRITE : / ls_komv-knumv,
            ls_komv-kposn,
            ls_komv-kschl,
            ls_komv-kwert.
ENDLOOP.
*out = cl_demo_output=>new( )->write_data( it_ekpo ).

out->write_data( it_komv ).

* Using LOOP and Work area
*out->write_text( 'Using LOOP and Work area:' ).
WRITE : / 'Using LOOP and Work area:'.
DATA st_ekpox    LIKE LINE OF it_ekpo.

LOOP AT it_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).

  st_ekpox = <fs_ekpo>.

  AT NEW ebelp.
    LOOP AT it_komv ASSIGNING FIELD-SYMBOL(<fs_komv>)
      USING KEY key_kposn
      WHERE kposn EQ st_ekpox-ebelp.

      <fs_ekpo>-netwr = <fs_ekpo>-netwr + <fs_komv>-kwert.

    ENDLOOP.
  ENDAT.

ENDLOOP.

LOOP AT it_ekpo INTO ls_ekpo.
  WRITE : ls_ekpo-ebeln,
          ls_ekpo-ebelp,
          ls_ekpo-netwr.
ENDLOOP.
*out->write_data( it_ekpo ).

* Using REDUCE
*out->write_text( 'Using REDUCE:' ).
WRITE : / 'Using REDUCE:'.
LOOP AT it_ekpo
  ASSIGNING FIELD-SYMBOL(<fs_ekpo2>).
  <fs_ekpo2>-netwr = REDUCE netwr( INIT val TYPE netwr
                                  FOR wa6 IN
                                  FILTER #( it_komv
                                            USING KEY key_kposn
                                            WHERE kposn EQ CONV #( <fs_ekpo2>-ebelp ) )
                                  NEXT val = val + wa6-kwert ).
ENDLOOP.
LOOP AT it_ekpo INTO ls_ekpo.
  WRITE : ls_ekpo-ebeln,
          ls_ekpo-ebelp,
          ls_ekpo-netwr.
ENDLOOP.
*out->write_data( it_ekpo )->display( ).

* REDUCE |UNTIL|
TYPES : BEGIN OF ty_str,
          str TYPE string,
        END OF ty_str.
DATA(result) = REDUCE string( INIT text = `Count up:`
                              FOR n = 1 UNTIL n > 10
                              NEXT text = text && | { n }| ).
WRITE : / 'RESULT', result.

DATA(result2) =
  REDUCE string( INIT text = `Count up:`
                 FOR n = 1 UNTIL n > 11
                 NEXT text = |{ n }| ).

WRITE : / 'RESULT2', result2.
*output->display( ).

SKIP.

*FOR ... WHILE
WRITE : / 'FOR ... WHILE'.

DATA itab10 TYPE STANDARD TABLE OF ty_itab WITH EMPTY KEY.
itab10 = VALUE #( FOR j = 1 WHILE j <= 10 ( i = j ) ).
*cl_demo_output=>display( itab10 ).
WRITE : / 'ITAB10 TABLE FILLED WITH FOR ... WHILE'.
WRITE : / '           I'.
LOOP AT itab10 INTO DATA(ls_itab10).
  WRITE : / ls_itab10-i.
ENDLOOP.

SKIP.
*FOR ... UNTIL
WRITE : / 'FOR ... UNTIL'.
itab10 = VALUE #( FOR j = 1 UNTIL j > 10 ( i = j ) ).
*cl_demo_output=>display( itab10 ).
WRITE : / 'ITAB10 TABLE FILLED WITH FOR ... UNTIL'.
WRITE : / '           I'.
LOOP AT itab10 INTO ls_itab10.
  WRITE : / ls_itab10-i.
ENDLOOP.

SKIP.
*Predicative Method Calls
WRITE : / 'Predicative Method Calls'.
IF cl_abap_demo_services=>is_production_system( ).

  WRITE : / 'This demo cannot be executed in a production system' .
  LEAVE PROGRAM.

ELSE.
  WRITE : / 'This demo can be executed in a non production system'.
ENDIF.

SKIP.
*New Boolean Function
WRITE : / 'New Boolean Function'.
WRITE : 'IF xsdbool( 1 = 2 ) = abap_false'.
IF xsdbool( 1 = 2 ) = abap_false.
  WRITE : / 'YES'.
*  cl_demo_output=>display_text( 'yes' ).
ELSE.
  WRITE : / 'NO'.
*  cl_demo_output=>display_text( 'no' ).
ENDIF.

SKIP.
*COND
WRITE : / 'COND'.
DATA(time) = COND string(
               WHEN sy-timlo < '120000' THEN
                 |{ sy-timlo TIME = ISO } AM|
               WHEN sy-timlo > '120000' THEN
                 |{ CONV t( CONV i( sy-timlo ) )
                    TIME = ISO } PM|
               WHEN sy-timlo = '120000' THEN
                 |High Noon|
               ELSE
                 |ELSE| ) .
WRITE : / 'TIME', time.
SKIP.
*SWITCH
WRITE : / 'SWITCH'.
DATA(text2) = SWITCH #( sy-langu
                    WHEN 'D' THEN `DE`
                    WHEN 'E' THEN `EN`
                    ELSE `langu_not_supported` ).

WRITE : / 'TEXT2', text2.

CLASS zcl_student IMPLEMENTATION.
  METHOD constructor.
    WRITE : / 'zcl_student constructor'.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_my_screen_message IMPLEMENTATION.
  METHOD display.
    WRITE : im_error.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_purorder IMPLEMENTATION.
  METHOD get_items.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_car_factory IMPLEMENTATION.
  METHOD build_new_car.
    eo_mercedes_benz = NEW zcl_mercedes_benz( ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_mercedes_benz IMPLEMENTATION.
  METHOD constructor.
    WRITE : / 'MERCEDES BENZ CONSTRUCTOR'.
  ENDMETHOD.
ENDCLASS.