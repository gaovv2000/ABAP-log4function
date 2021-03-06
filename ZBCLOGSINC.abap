*&---------------------------------------------------------------------*
*& author : William.Gao / ¸ßÎ¡Î¡
*& include 
*&---------------------------------------------------------------------*
  TYPE-POOLS: sydes.

  DATA : gt_logdatas  LIKE TABLE OF ztbclogs_rlgd
       , gt_paras     LIKE TABLE OF zsfuncparas
       , gs_logheader LIKE          ztbclogs_rlgk
       , gs_logdata   LIKE          zsfuncparas
       , gs_para      LIKE          zsfuncparas
       , g_funcname   TYPE          rs38l_fnam
       , g_id         TYPE          guid_32
       , g_serial     TYPE          sytabix   VALUE 1
       , g_fsname     TYPE          string
       , g_active
       .

  FIELD-SYMBOLS
       <fs_data> .

*&---------------------------------------------------------------------*
*&      Form  frm_build_detial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_DATA)  text
*      -->REF            text
*      -->TO             text
*      -->DATA           text
*      -->PT_DETIAL      text
*      -->ENDFORM        text
*----------------------------------------------------------------------*
  FORM frm_build_detial USING value(p_id)
                              value(p_s_para) TYPE zsfuncparas
                              value(p_data)
                     CHANGING pt_detial TYPE ANY TABLE
                      D .

    DATA
      : lr_typedescr  TYPE REF TO cl_abap_typedescr
      , lr_tabdescr   TYPE REF TO cl_abap_tabledescr
      , l_type        TYPE        rs38l_typ
      , l_index       TYPE        sytabix
      , ls_rlgd       TYPE        ztbclogs_rlgd
      .

*  FIELD-SYMBOLS
*    : <fs_data>
*    , <fs_line>
*    , <fs_tab>  TYPE STANDARD TABLE
*    .


    CHECK p_data IS NOT INITIAL.

    "------------- get kind type -----------------

    PERFORM frm_get_typedescr USING p_data CHANGING lr_typedescr l_type.
    g_serial            = 1                    . "reset the element index
    ls_rlgd-zlogsnum    = p_id                 .
    ls_rlgd-paramtype   = p_s_para-paramtype   .
    ls_rlgd-zparameter  = p_s_para-parameter   .
    ls_rlgd-zindex      = g_serial             .



    "------------- process different type --------------
    CASE lr_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_elem. "------------------

        PERFORM frm_set_elem USING    p_data
                                      cl_abap_typedescr=>kind_elem
                                      p_s_para-structure  "l_type
                                      g_serial
                                      ''  " field name
                                      0
                             CHANGING ls_rlgd
                                      pt_detial.

      WHEN cl_abap_typedescr=>kind_struct."------------------

        PERFORM frm_set_struc USING   p_data
                                      cl_abap_typedescr=>kind_struct
                                      p_s_para-structure  "l_type
                                      g_serial
                                      ''  " field name
                                      0
                             CHANGING ls_rlgd
                                      pt_detial.



      WHEN cl_abap_typedescr=>kind_table."------------------

        PERFORM frm_set_table     USING p_data
                                        cl_abap_typedescr=>kind_table
                                        p_s_para-structure  "l_type
                                        g_serial
                                        ''  " field name
                                        0
                               CHANGING ls_rlgd
                                        pt_detial[].

      WHEN OTHERS.
    ENDCASE.


  ENDFORM.                    "frm_build_detial


*&---------------------------------------------------------------------*
*&      Form  FRM_SET_ELEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_data  text
*      -->P_LR_TYPEDESCR_>KIND  text
*      -->P_L_TYPE  text
*      -->P_G_SERIAL  text
*      -->P_0109   text
*      <--P_LS_RLGD  text
*----------------------------------------------------------------------*
  FORM frm_set_elem  USING   p_data
                              p_kind
                              p_type
                              p_serial
                              p_fieldname
                              p_parent_ix
                     CHANGING p_s_rlgd   TYPE ztbclogs_rlgd
                              p_t_detial TYPE STANDARD TABLE.

    CHECK p_data IS NOT INITIAL.

    p_s_rlgd-para_kind  = p_kind.
    p_s_rlgd-zindex     = p_serial.
    p_s_rlgd-name_feld  = p_fieldname .
    p_s_rlgd-value      = p_data.
    p_s_rlgd-zparent_ix = p_parent_ix.
    p_s_rlgd-zreftype   = p_type.

    APPEND p_s_rlgd  TO p_t_detial.
    ADD    1         TO g_serial.

  ENDFORM.                    " FRM_SET_ELEM


*&---------------------------------------------------------------------*
*&      Form  frm_set_struc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA       text
*      -->P_KIND       text
*      -->P_TYPE       text
*      -->P_SERIAL     text
*      -->P_FIELDNAME  text
*      -->P_PARENT_IX  text
*      -->P_S_RLGD     text
*      -->P_T_DETIAL   text
*----------------------------------------------------------------------*
  FORM frm_set_struc   USING  p_data
                              p_kind
                              p_type
                              p_serial
                              p_fieldname
                              p_parent_ix
                     CHANGING p_s_rlgd   TYPE ztbclogs_rlgd
                              p_t_detial TYPE STANDARD TABLE .


    DATA
      : ls_compdescr  TYPE        abap_compdescr
      , l_index       TYPE        sy-tabix
      , lr_typedescr  TYPE REF TO cl_abap_typedescr
      , lr_strudescr  TYPE REF TO cl_abap_structdescr
      , l_type        TYPE        rs38l_typ
      .

    FIELD-SYMBOLS
      <fs_data>.
    "---------- add STRUCT_BEGIN line -------------------

    l_index             = p_serial.
    p_s_rlgd-zparent_ix = p_parent_ix.

    PERFORM frm_set_elem USING    '%STRUCT_BEGIN%'
                                  p_kind
                                  p_type
                                  p_serial
                                  p_fieldname
                                  p_parent_ix
                         CHANGING p_s_rlgd
                                  p_t_detial.


    "---------- add field iteim -----------------

    lr_strudescr ?= cl_abap_structdescr=>describe_by_data( p_data ).
*
    LOOP AT lr_strudescr->components INTO  ls_compdescr .

      ASSIGN COMPONENT ls_compdescr-name OF STRUCTURE p_data TO <fs_data>.
      CHECK <fs_data> IS ASSIGNED.

      lr_typedescr ?= cl_abap_typedescr=>describe_by_data( <fs_data> ).

      CHECK lr_typedescr IS NOT INITIAL.

      SEARCH lr_typedescr->absolute_name FOR '\TYPE='.

      IF sy-subrc = 0.
        sy-fdpos = sy-fdpos + STRLEN( '\TYPE=' ) .
        l_type   = lr_typedescr->absolute_name+sy-fdpos.
      ENDIF.

      "----------------------------------
      CASE lr_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_elem.

          PERFORM frm_set_elem USING    <fs_data>
                                        cl_abap_typedescr=>kind_elem
                                        l_type
                                        g_serial
                                        ls_compdescr-name
                                        l_index
                               CHANGING p_s_rlgd
                                      p_t_detial.


        WHEN cl_abap_typedescr=>kind_struct.

          PERFORM frm_set_struc USING   <fs_data>
                                        cl_abap_typedescr=>kind_struct
                                        l_type
                                        g_serial
                                        ls_compdescr-name
                                        l_index
                               CHANGING p_s_rlgd
                                        p_t_detial[].

        WHEN cl_abap_typedescr=>kind_table.

          PERFORM frm_set_table USING   <fs_data>
                                        cl_abap_typedescr=>kind_table
                                        l_type
                                        g_serial
                                        ls_compdescr-name
                                        l_index
                               CHANGING p_s_rlgd
                                        p_t_detial[].



        WHEN OTHERS.
      ENDCASE.

      UNASSIGN <fs_data>.

    ENDLOOP.


    "---------- add STRUCT_BEGIN line -------------------

    l_index             = p_serial.
    p_s_rlgd-zparent_ix = p_parent_ix.

    PERFORM frm_set_elem USING    '%STRUCT_END%'
                                  p_kind
                                  p_type
                                  p_serial
                                  p_fieldname
                                  p_parent_ix
                         CHANGING p_s_rlgd
                                  p_t_detial.


  ENDFORM.                    "frm_set_struc


*&---------------------------------------------------------------------*
*&      Form  frm_set_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA       text
*      -->P_KIND       text
*      -->P_TYPE       text
*      -->P_SERIAL     text
*      -->P_FIELDNAME  text
*      -->P_PARENT_IX  text
*      -->P_S_RLGD     text
*      -->P_T_DETIAL   text
*----------------------------------------------------------------------*
  FORM frm_set_table  USING  p_data
                              p_kind
                              p_type
                              p_serial
                              p_fieldname
                              p_parent_ix
                     CHANGING p_s_rlgd   TYPE ztbclogs_rlgd
                              p_t_detial TYPE STANDARD TABLE .

    DATA
      : l_index        TYPE        sy-tabix
      , lr_typedescr   TYPE REF TO cl_abap_typedescr
      , lr_1stdescr    TYPE REF TO cl_abap_typedescr
      , ls_compdescr   TYPE        abap_compdescr
      , lr_structdescr TYPE REF TO cl_abap_structdescr
      , l_type         TYPE        rs38l_typ
      , BEGIN OF lt_comp OCCURS 0
      ,  name TYPE abap_compname
      ,  type TYPE rs38l_typ
      ,  kind TYPE abap_typecategory
      ,  END OF lt_comp
      .

    FIELD-SYMBOLS
        : <fs_data>
        , <fs_line>
        , <fs_tab>  TYPE STANDARD TABLE
        .

    CHECK  p_data  IS NOT INITIAL .
    ASSIGN p_data  TO <fs_tab>.


    "---------- add table_begin line -------------------
    l_index             = p_serial.
    p_s_rlgd-zparent_ix = p_parent_ix.

    PERFORM frm_set_elem USING    '%TABLE_BEGIN%'
                                  p_kind
                                  p_type
                                  p_serial
                                  p_fieldname
                                  p_parent_ix
                         CHANGING p_s_rlgd
                                  p_t_detial.

    LOOP AT <fs_tab> ASSIGNING <fs_line>.

      PERFORM frm_get_typedescr USING <fs_line> CHANGING lr_typedescr l_type.

      "----------------------------------
      CASE lr_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_elem.

          PERFORM frm_set_elem USING    <fs_line>
                                        cl_abap_typedescr=>kind_elem
                                        l_type
                                        g_serial
                                        ''
                                        l_index
                               CHANGING p_s_rlgd
                                        p_t_detial.


        WHEN cl_abap_typedescr=>kind_struct.

          PERFORM frm_set_struc USING   <fs_line>
                                        cl_abap_typedescr=>kind_struct
                                        lt_comp-type
                                        g_serial
                                        lt_comp-name
                                        l_index
                               CHANGING p_s_rlgd
                                        p_t_detial[].


        WHEN cl_abap_typedescr=>kind_table.

          PERFORM frm_set_table USING   <fs_line>
                                        lr_typedescr->kind
                                        l_type
                                        g_serial
                                        '' "ls_compdescr-name
                                        l_index
                               CHANGING p_s_rlgd
                                        p_t_detial[].

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    "---------- add table_begin line -------------------

    l_index             = p_serial.
    p_s_rlgd-zparent_ix = p_parent_ix.

    PERFORM frm_set_elem USING    '%TABLE_END%'
                                  p_kind
                                  p_type
                                  p_serial
                                  p_fieldname
                                  p_parent_ix
                         CHANGING p_s_rlgd
                                  p_t_detial.


  ENDFORM.                    "frm_set_table


*&---------------------------------------------------------------------*
*&      Form  frm_get_typedescr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_data       text
*      -->LR_TYPEDESCR  text
*      -->L_TYPE        text
*----------------------------------------------------------------------*
  FORM frm_get_typedescr USING p_data
                      CHANGING lr_typedescr TYPE REF TO cl_abap_typedescr
                               l_type.

    lr_typedescr ?= cl_abap_typedescr=>describe_by_data( p_data ).

    CHECK lr_typedescr IS NOT INITIAL.

    SEARCH lr_typedescr->absolute_name FOR '\TYPE='.

    IF sy-subrc = 0.
      sy-fdpos = sy-fdpos + STRLEN( '\TYPE=' ) .
      l_type   = lr_typedescr->absolute_name+sy-fdpos.
    ENDIF.


  ENDFORM.                    "frm_get_typedescr

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FUNCNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM frm_get_funcname CHANGING p_funcname.

    DATA: lt_callstack TYPE abap_callstack,
          ls_callstack TYPE abap_callstack_line,
          lv_fname TYPE rs38l_fnam.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = lt_callstack.
*  delete     lt_callstack where mainprogram = sy-repid.
    READ TABLE lt_callstack INTO ls_callstack WITH KEY blocktype = 'FUNCTION'.
    IF sy-subrc = 0.
      p_funcname = ls_callstack-blockname.
    ENDIF.

  ENDFORM.                    " FRM_GET_FUNCNAME

*&---------------------------------------------------------------------*
*&      Form  frm_get_funcname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FUNCNAME text
*----------------------------------------------------------------------*
  FORM  frm_get_funcact CHANGING p_active.
    CLEAR p_active .

    SELECT SINGLE active
      INTO g_active
      FROM ztbclogs_act
     WHERE name = g_funcname.
  ENDFORM.                    "frm_get_funcname
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FUNCPARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FUNCNAME  text
*      <--P_ET_PARAS[]  text
*----------------------------------------------------------------------*
  FORM frm_get_funcpara  USING    p_i_funcname
                         CHANGING p_et_paras TYPE STANDARD TABLE.

    REFRESH p_et_paras.

    SELECT parameter
           paramtype
           structure
      INTO TABLE p_et_paras
      FROM fupararef
     WHERE funcname = p_i_funcname
       AND r3state  = 'A'
       .
  ENDFORM.                    " FRM_GET_FUNCPARA



  "----------------------------------------------------------------------------
  DEFINE save_para_data.


    refresh gt_logdatas.


    perform frm_get_funcname changing g_funcname .
    perform frm_get_funcact  changing g_active   .

    if g_active = 'X'.
      perform frm_get_funcpara using    g_funcname changing gt_paras.

      delete gt_paras where paramtype = 'E'.

      call function 'GUID_CREATE'
        importing
          ev_guid_32 = g_id.


      loop at gt_paras into gs_para.

        case gs_para-paramtype. " [I:import,E:export,C:changing,T:table]
          when 'I' or 'C'.      g_fsname = gs_para-parameter.
          when 'T'.             concatenate gs_para-parameter  '[]' into g_fsname.
          when others.          continue.
        endcase.

        assign (g_fsname) to <fs_data>.
        perform frm_build_detial using g_id gs_para <fs_data> changing gt_logdatas   .
        unassign <fs_data>.
      endloop.

      gs_logheader-zlogsnum        = g_id.
      gs_logheader-plug_type       = '0'.           "inbound
      gs_logheader-funcname        = g_funcname .
      gs_logheader-erdat           = sy-datum .
      gs_logheader-erzet           = sy-uzeit .
      gs_logheader-ernam           = sy-uname .
*    gs_logheader-final_stauts    =
*    gs_logheader-destination     = g_i
*    gs_logheader-externalid      = g_i
*    gs_logheader-out_url         = g_i

      modify ztbclogs_rlgk from       gs_logheader.
      modify ztbclogs_rlgd from table gt_logdatas .
    endif. "-------- end if g_active = 'X'. --------------

  END-OF-DEFINITION.
