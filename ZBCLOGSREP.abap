*&---------------------------------------------------------------------*
*& Report  ZBCLOGSREP
*&
*&---------------------------------------------------------------------*
*& author : William.Gao¡¡£¯¡¡¸ßÎ¡Î¡
*& ##### ¡¡¡¡
*&---------------------------------------------------------------------*

REPORT  zbclogsrep.
TYPE-POOLS:slis,icon.
TABLES ztbclogs_rlgk.




DATA
  : gt_rlgk   LIKE TABLE OF ztbclogs_rlgk
  , gt_rlgd   LIKE TABLE OF ztbclogs_rlgd
  , gt_fupa   LIKE TABLE OF fupararef
  , gt_para   LIKE TABLE OF zsfuncparas
  , gs_layout TYPE          slis_layout_alv
  , g_title   TYPE          lvc_title
  .

SELECT-OPTIONS
  : s_zlognm          FOR ztbclogs_rlgk-zlogsnum
  , s_plugtp          FOR ztbclogs_rlgk-plug_type
  , s_funcnm          FOR ztbclogs_rlgk-funcname
  , s_stauts          FOR ztbclogs_rlgk-final_stauts
  , s_dest            FOR ztbclogs_rlgk-destination
  , s_extern          FOR ztbclogs_rlgk-externalid
  , s_outurl          FOR ztbclogs_rlgk-out_url
  , s_erdat           FOR ztbclogs_rlgk-erdat
  , s_erzet           FOR ztbclogs_rlgk-erzet
  , s_ernam           FOR ztbclogs_rlgk-ernam
  .


START-OF-SELECTION.

  SELECT *
    INTO TABLE gt_rlgk
    FROM ztbclogs_rlgk
   WHERE zlogsnum       IN  s_zlognm
     AND plug_type      IN  s_plugtp
     AND funcname       IN  s_funcnm
     AND final_stauts   IN  s_stauts
     AND destination    IN  s_dest
     AND externalid     IN  s_extern
     AND out_url        IN  s_outurl
     AND erdat          IN  s_erdat
     AND erzet          IN  s_erzet
     AND ernam          IN  s_ernam
    .

END-OF-SELECTION.

  IF gt_rlgk IS INITIAL .
    MESSAGE s429(mo) DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    PERFORM 1frm_loglist_alv.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 1frm_loglist_alv.

  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.

  g_title = 'Logs List'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = 'ZBCLOGSREP'
      is_layout               = gs_layout
      i_grid_title            = g_title
      i_structure_name        = 'ZTBCLOGS_RLGK'
      i_callback_user_command = '2FRM_LOGLIST_COMMAND'
    TABLES
      t_outtab                = gt_rlgk
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_ALV_SHOW


*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 2frm_loglist_command  USING r_ucomm     LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  DATA
    : lt_para     LIKE TABLE OF zsfuncparas
    , l_parameter LIKE          zsfuncparas-parameter
    .

  CASE r_ucomm.
    WHEN '&IC1'.
      REFRESH gt_para.
      PERFORM 3frm_logdata_detial USING rs_selfield-tabindex
                               CHANGING gt_para.
      PERFORM 4frm_logdata_alv    USING gt_para.
    WHEN OTHERS.
  ENDCASE.



ENDFORM.                    "FRM_USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  FRM_DETIAL_LOGDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABIX    text
*----------------------------------------------------------------------*
FORM 3frm_logdata_detial USING p_tabix
                      CHANGING pt_para TYPE STANDARD TABLE.

  DATA
    : ls_rlgk  LIKE  ztbclogs_rlgk
    , ls_rlgd  LIKE  ztbclogs_rlgd
    , ls_para  LIKE  zsfuncparas
    , ls_fupa  LIKE  fupararef
    .


  g_title = 'Parameter Data'.

  READ TABLE gt_rlgk INTO ls_rlgk INDEX p_tabix.

  CHECK sy-subrc = 0.

  SELECT *
    INTO TABLE gt_rlgd
    FROM ztbclogs_rlgd
   WHERE zlogsnum = ls_rlgk-zlogsnum
    .

  SORT gt_rlgd BY zlogsnum  zindex.

  SELECT *
    INTO TABLE gt_fupa
    FROM fupararef
   WHERE funcname = ls_rlgk-funcname
     AND r3state  = 'A'
     .

  LOOP AT gt_fupa INTO ls_fupa.

    READ TABLE gt_rlgd INTO ls_rlgd WITH KEY zparameter = ls_fupa-parameter.

    CHECK sy-subrc = 0.

    ls_para-parameter = ls_fupa-parameter  .
    ls_para-paramtype = ls_fupa-paramtype  .

    CASE ls_rlgd-para_kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_para-structure = ls_rlgd-value.
      WHEN cl_abap_typedescr=>kind_struct OR
           cl_abap_typedescr=>kind_table   .
        ls_para-structure = icon_icon_list .
      WHEN OTHERS.
    ENDCASE.

    APPEND ls_para TO pt_para.
    CLEAR  ls_para .
  ENDLOOP.



ENDFORM.                    "frm_detial_show
*&---------------------------------------------------------------------*
*&      Form  FRM_DETIAL_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PARA  text
*----------------------------------------------------------------------*
FORM 4frm_logdata_alv  USING   pt_para TYPE STANDARD TABLE.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = 'ZBCLOGSREP'
      is_layout               = gs_layout
      i_grid_title            = g_title
      i_callback_user_command = '5FRM_LOGDATA_COMMAND'
      i_structure_name        = 'ZSFUNCPARAS'
    TABLES
      t_outtab                = pt_para
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_SHOW_DETIAL

*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMDETIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM 5frm_logdata_command   USING r_ucomm     LIKE sy-ucomm
                                  rs_selfield TYPE slis_selfield.
  DATA
    : ls_para       LIKE zsfuncparas
    , ls_rlgd       LIKE ztbclogs_rlgd
    , lt_para       LIKE TABLE OF zsfuncparas
    , lt_temp       LIKE TABLE OF zsfuncparas
    , l_parameter   LIKE zsfuncparas-parameter
    .

  CASE r_ucomm.
    WHEN '&IC1'.
      CHECK   rs_selfield-fieldname   = 'STRUCTURE'     .
      CHECK   rs_selfield-value       = icon_icon_list  .

      READ TABLE gt_para INTO ls_para INDEX    rs_selfield-tabindex.          CHECK sy-subrc = 0.
      IF ls_para-parameter CS '-'.
        SPLIT ls_para-parameter AT '-' INTO ls_para-parameter ls_para-parameter .
        READ TABLE gt_rlgd INTO ls_rlgd WITH KEY name_feld  = ls_para-parameter .CHECK sy-subrc = 0.
      ELSE.
        READ TABLE gt_rlgd INTO ls_rlgd WITH KEY zparameter = ls_para-parameter .CHECK sy-subrc = 0.
      ENDIF.

      CASE ls_rlgd-para_kind .
        WHEN  cl_abap_typedescr=>kind_table.
          PERFORM 6frm_logtable_alv   USING rs_selfield-tabindex.
        WHEN cl_abap_typedescr=>kind_struct.

          PERFORM 7frm_logstruct_alv  USING rs_selfield-tabindex
                                   CHANGING lt_para.
          lt_temp = gt_para.
          gt_para = lt_para.
          PERFORM 4frm_logdata_alv    USING lt_para.
          gt_para = lt_temp.
        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    "FRM_USER_COMMDETIAL
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DETIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*----------------------------------------------------------------------*
FORM 6frm_logtable_alv  USING    p_tabix.

  DATA
    : ls_para       LIKE        zsfuncparas
    , ls_fupa       LIKE        fupararef
    , ls_rlgd       LIKE        ztbclogs_rlgd
    , ls_struc      TYPE REF TO data
    , lt_dyn_table  TYPE REF TO data
    , lr_typedescr  TYPE REF TO cl_abap_typedescr
    , lt_lvc_fcat   TYPE        lvc_t_fcat
    , l_stname      LIKE        dd02l-tabname
    .

  DATA
    : l_ref_tab   TYPE REF TO data
    , l_ref_wa    TYPE REF TO data
    .

  FIELD-SYMBOLS
    : <fs_tab> TYPE STANDARD TABLE
    , <fs_wa>
    , <fs_val>
    .

  READ TABLE gt_para INTO ls_para INDEX p_tabix.

  CHECK sy-subrc = 0.

  CONCATENATE 'Table Contant :'
              ls_para-parameter
         INTO g_title.


  IF ls_para-parameter CS '-'.
    SPLIT ls_para-parameter AT '-' INTO ls_para-parameter ls_para-parameter.
    READ TABLE gt_rlgd INTO ls_rlgd WITH KEY name_feld  = ls_para-parameter.CHECK sy-subrc = 0.
  ELSE.
    READ TABLE gt_rlgd INTO ls_rlgd WITH KEY zparameter  = ls_para-parameter.CHECK sy-subrc = 0.
  ENDIF.

  l_stname = ls_rlgd-zreftype.

  "----------------- create dyn table ----------------------------------
  DATA
    : ls_ddtypes LIKE ddtypes .

  SELECT SINGLE *
    INTO ls_ddtypes
    FROM ddtypes
   WHERE typename = l_stname
     AND state    = 'A'
     .

  IF ls_ddtypes-typekind = 'TTYP'.
    SELECT SINGLE rowtype
      INTO l_stname
      FROM dd40l
     WHERE typename = l_stname
       AND as4local = 'A'.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = l_stname
    CHANGING
      ct_fieldcat            = lt_lvc_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.

  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_lvc_fcat
    IMPORTING
      ep_table        = l_ref_tab.

  ASSIGN l_ref_tab->* TO <fs_tab>.

  CREATE DATA l_ref_wa LIKE LINE OF <fs_tab>.
  ASSIGN l_ref_wa->* TO <fs_wa>.



  "----------------- fill data -------------------------------------
  DATA
    : l_zparent_ix  TYPE ztbclogs_rlgd-zparent_ix
    .

  LOOP AT gt_rlgd INTO ls_rlgd FROM ls_rlgd-zindex. "WHERE zparameter = ls_fupa-parameter.

    IF ls_rlgd-name_feld IS NOT INITIAL.
      ASSIGN COMPONENT ls_rlgd-name_feld OF STRUCTURE <fs_wa> TO <fs_val>.

      IF sy-subrc = 0 .
        <fs_val> = ls_rlgd-value.
      ELSE.
        IF ls_rlgd-value CP '%*_BEGIN%'.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_rlgd-value = '%STRUCT_END%'.
      APPEND <fs_wa> TO <fs_tab>.
      CLEAR  <fs_wa> .
    ENDIF.

    IF ls_rlgd-value = '%TABLE_END%'.
      EXIT.
    ENDIF.

  ENDLOOP.

  "--------------- show data --------------------------------------
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = 'ZBCLOGSREP'
      is_layout          = gs_layout
      i_grid_title       = g_title
      i_structure_name   = l_stname
    TABLES
      t_outtab           = <fs_tab>
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_SHOW_DETIAL_DATA


*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_STRUCT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*----------------------------------------------------------------------*
FORM 7frm_logstruct_alv USING p_tabix
                     CHANGING pt_para TYPE STANDARD TABLE.

  DATA
    : ls_para       LIKE       zsfuncparas
    , ls_fupa       LIKE       fupararef
    , ls_rlgd       LIKE       ztbclogs_rlgd
    , l_index       TYPE       i
    .


  READ TABLE gt_para INTO ls_para INDEX p_tabix.
  CHECK sy-subrc = 0.

  READ TABLE gt_fupa INTO ls_fupa WITH KEY parameter = ls_para-parameter .
  CHECK sy-subrc = 0.

  READ TABLE gt_rlgd INTO ls_rlgd WITH KEY zparameter = ls_fupa-parameter.
  CHECK sy-subrc = 0.


  CONCATENATE 'Structure Contant :'
              ls_para-parameter
         INTO g_title.


  l_index = ls_rlgd-zindex.

  LOOP AT gt_rlgd INTO ls_rlgd WHERE zparent_ix = l_index.
    CONCATENATE ls_rlgd-zparameter '-'
                ls_rlgd-name_feld
           INTO ls_para-parameter .

    ls_para-paramtype    = ls_rlgd-para_kind    .

    IF ls_rlgd-value CP '%*_BEGIN%' OR
       ls_rlgd-value CP '%*_END%'   .
      ls_para-structure  = icon_icon_list.
    ELSE.
      ls_para-structure  = ls_rlgd-value.
    ENDIF.

    COLLECT ls_para INTO pt_para.
  ENDLOOP.


ENDFORM.                    " FRM_SHOW_STRUCT_DATA
