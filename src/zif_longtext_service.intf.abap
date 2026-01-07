INTERFACE zif_longtext_service PUBLIC.

  " Text key structure
  TYPES: BEGIN OF ty_text_key,
           textobject TYPE tdobject,
           textname   TYPE tdobname,
           textid     TYPE tdid,
           language   TYPE spras,
         END OF ty_text_key,
         ty_text_keys TYPE STANDARD TABLE OF ty_text_key WITH EMPTY KEY.

  " Text result structure
  TYPES: BEGIN OF ty_text_result,
           textobject  TYPE tdobject,
           textname    TYPE tdobname,
           textid      TYPE tdid,
           language    TYPE spras,
           title       TYPE tdtitle,
           createdby   TYPE tdluser,
           createddate TYPE tdfdate,
           changedby   TYPE tdluser,
           changeddate TYPE tdldate,
           longtext    TYPE string,
         END OF ty_text_result,
         ty_text_results TYPE STANDARD TABLE OF ty_text_result WITH EMPTY KEY.

  " Filter ranges
  TYPES: ty_r_textobject TYPE RANGE OF tdobject,
         ty_r_textname   TYPE RANGE OF tdobname,
         ty_r_textid     TYPE RANGE OF tdid,
         ty_r_language   TYPE RANGE OF spras.

  " Paging structure
  TYPES: BEGIN OF ty_paging,
           offset TYPE i,
           limit  TYPE i,
         END OF ty_paging.

  " Query result with count
  TYPES: BEGIN OF ty_query_result,
           results     TYPE ty_text_results,
           total_count TYPE i,
         END OF ty_query_result.

  " Read texts by keys (for SADL exit)
  METHODS read_texts
    IMPORTING
      it_keys            TYPE ty_text_keys
      iv_include_content TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_results)  TYPE ty_text_results.

  " Read texts with filters and paging (for RAP query)
  METHODS read_texts_with_paging
    IMPORTING
      ir_textobject      TYPE ty_r_textobject OPTIONAL
      ir_textname        TYPE ty_r_textname OPTIONAL
      ir_textid          TYPE ty_r_textid OPTIONAL
      ir_language        TYPE ty_r_language OPTIONAL
      is_paging          TYPE ty_paging OPTIONAL
      iv_include_content TYPE abap_bool DEFAULT abap_true
      iv_count_only      TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rs_result)   TYPE ty_query_result.

ENDINTERFACE.
