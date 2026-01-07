CLASS zcl_rap_longtext_exit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.

    " Constructor with optional dependency injection
    METHODS constructor
      IMPORTING
        io_service TYPE REF TO zif_longtext_service OPTIONAL.

  PRIVATE SECTION.
    DATA mo_service TYPE REF TO zif_longtext_service.

ENDCLASS.

CLASS zcl_rap_longtext_exit IMPLEMENTATION.

  METHOD constructor.
    IF io_service IS BOUND.
      mo_service = io_service.
    ELSE.
      mo_service = NEW zcl_longtext_service( ).
    ENDIF.
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    et_requested_orig_elements = VALUE #(
      ( `TEXTOBJECT` )
      ( `TEXTNAME` )
      ( `TEXTID` )
      ( `LANGUAGE` )
    ).
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~calculate.
    " Build keys from original data
    DATA lt_keys TYPE zif_longtext_service=>ty_text_keys.

    LOOP AT it_original_data ASSIGNING FIELD-SYMBOL(<orig>).
      ASSIGN COMPONENT 'TEXTOBJECT' OF STRUCTURE <orig> TO FIELD-SYMBOL(<obj>).
      ASSIGN COMPONENT 'TEXTNAME' OF STRUCTURE <orig> TO FIELD-SYMBOL(<name>).
      ASSIGN COMPONENT 'TEXTID' OF STRUCTURE <orig> TO FIELD-SYMBOL(<id>).
      ASSIGN COMPONENT 'LANGUAGE' OF STRUCTURE <orig> TO FIELD-SYMBOL(<lang>).

      IF <obj> IS ASSIGNED AND <name> IS ASSIGNED AND <id> IS ASSIGNED AND <lang> IS ASSIGNED.
        APPEND VALUE #(
          textobject = <obj>
          textname   = <name>
          textid     = <id>
          language   = <lang>
        ) TO lt_keys.
      ENDIF.
    ENDLOOP.

    " Read texts via service
    DATA(lt_results) = mo_service->read_texts( lt_keys ).

    " Map results back to calculated data
    LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<result>).
      DATA(lv_idx) = sy-tabix.
      READ TABLE ct_calculated_data INDEX lv_idx ASSIGNING FIELD-SYMBOL(<calc>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'LONGTEXT' OF STRUCTURE <calc> TO FIELD-SYMBOL(<longtext>).
        IF sy-subrc = 0.
          <longtext> = <result>-longtext.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
