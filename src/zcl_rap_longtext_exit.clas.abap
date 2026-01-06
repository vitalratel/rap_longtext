CLASS zcl_rap_longtext_exit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
ENDCLASS.

CLASS zcl_rap_longtext_exit IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    et_requested_orig_elements = VALUE #(
      ( `TEXTOBJECT` )
      ( `TEXTNAME` )
      ( `TEXTID` )
      ( `LANGUAGE` )
    ).
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~calculate.
    TYPES: BEGIN OF original_type,
             textobject TYPE tdobject,
             textname   TYPE tdobname,
             textid     TYPE tdid,
             language   TYPE spras,
           END OF original_type.

    TYPES: BEGIN OF calculated_type,
             longtext TYPE string,
           END OF calculated_type.

    DATA lines TYPE tline_tab.

    LOOP AT it_original_data ASSIGNING FIELD-SYMBOL(<orig>).
      DATA(original) = CORRESPONDING original_type( <orig> ).
      READ TABLE ct_calculated_data INDEX sy-tabix ASSIGNING FIELD-SYMBOL(<calc>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = original-textid
          language = original-language
          name     = original-textname
          object   = original-textobject
        TABLES
          lines    = lines
        EXCEPTIONS
          OTHERS   = 8.

      IF sy-subrc = 0 AND lines IS NOT INITIAL.
        DATA stream_lines TYPE string_table.
        CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
          EXPORTING
            language     = original-language
            lf           = abap_true
          IMPORTING
            stream_lines = stream_lines
          TABLES
            itf_text     = lines.

        DATA(text) = concat_lines_of( table = stream_lines sep = ` ` ).

        DATA(calculated) = CORRESPONDING calculated_type( <calc> ).
        calculated-longtext = text.
        <calc> = CORRESPONDING #( calculated ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.