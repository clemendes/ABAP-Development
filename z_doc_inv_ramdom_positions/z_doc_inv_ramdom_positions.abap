*&---------------------------------------------------------------------&
*& Módule:    WM - Warehouse Management
*&---------------------------------------------------------------------*
*& Description: Generation of Inventory Documents
*               on current year  - Random Positions
*&---------------------------------------------------------------------*

REPORT z_doc_inv_ramdom_positions.

* TABELAS
TABLES: sscrfields, t302t.

* TIPOS
TYPE-POOLS icon.
TYPES:
  BEGIN OF ty_lagp,
    lgnum TYPE lagp-lgnum,
    lgtyp TYPE lagp-lgtyp,
    lgpla TYPE lagp-lgpla,
    lgber TYPE lagp-lgber,
    skzsi TYPE lagp-skzsi,
    ivivo TYPE lagp-ivivo,
  END OF ty_lagp,
  BEGIN OF ty_ZTAB001,
    lgnum      TYPE ZTAB001-lgnum,
    lgtyp      TYPE ZTAB001-lgtyp,
    lgpla      TYPE ZTAB001-lgpla,
    lgber      TYPE ZTAB001-lgber,
    ano        TYPE ZTAB001-ano,
    uname      TYPE ZTAB001-uname,
    dtcriacao  TYPE ZTAB001-dtcriacao,
    hrcriacao  TYPE ZTAB001-hrcriacao,
    docinv     TYPE ZTAB001-docinv,
    referencia TYPE ZTAB001-referencia,
  END OF ty_ZTAB001,
  BEGIN OF ty_zlagp,
    lgnum TYPE lagp-lgnum,
    lgtyp TYPE lagp-lgtyp,
    lgpla TYPE lagp-lgpla,
    lgber TYPE lagp-lgber,
    ref   TYPE ZTAB001-referencia,
  END OF ty_zlagp,
* AREAS DE ARMAZENAGEM
  BEGIN OF ty_areas,
         lgnum TYPE t302t-lgnum,
         lgtyp TYPE t302t-lgtyp,
         lgber TYPE t302t-lgber,
         lbert TYPE t302t-lbert,
  END OF ty_areas.

* ESTRUTURAS
DATA: it_return_tab TYPE ddshretval OCCURS 0,
      wa_return LIKE LINE OF it_return_tab.

* TABELAS INTERNAS
DATA:
  gt_lagp      TYPE TABLE OF ty_lagp,
  gt_ZTAB001 TYPE TABLE OF ty_ZTAB001,
  gt_rlagp     TYPE TABLE OF ty_zlagp,
  gt_possbloq  TYPE TABLE OF ty_lagp.
  DATA: it_t302t TYPE ty_areas OCCURS 0.

* VARIAVEIS GLOBAIS
DATA:
  gs_lagp      TYPE ty_lagp,
  gv_erro      TYPE abap_bool,
  functxt TYPE smp_dyntxt,
  gv_realiz    TYPE char10,
  gv_ano       TYPE pbr_rfyear,
  gv_ttpos     TYPE seqn5,
  gv_posger    TYPE seqn5,
  lv_minps     TYPE t340d-minps,
  gv_lgber    TYPE lgber.

* VARIAVEIS ALV
DATA:
  gt_alv       TYPE TABLE OF ty_lagp,
  gr_table     TYPE REF TO cl_salv_table,
  gr_display   TYPE REF TO cl_salv_display_settings,
  gr_functions TYPE REF TO cl_salv_functions,
  gr_columns   TYPE REF TO cl_salv_columns_table.

* PARAMETERS
SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE text-002.
  PARAMETERS: p_lgnum  TYPE lagp-lgnum,
              p_lgtyp  TYPE lagp-lgtyp,
              p_lgber TYPE lagp-lgber,
              p_dtcont TYPE sy-datum DEFAULT sy-datum,
              p_qtdpos TYPE seqn5.
SELECTION-SCREEN END OF BLOCK bl01.

INITIALIZATION.
  functxt-icon_id   = icon_incomplete.
  functxt-quickinfo = 'Limpar campos'.
  functxt-icon_text = 'Limpar campos'.
  sscrfields-functxt_01 = functxt.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name EQ 'P_LGBER'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON p_lgnum.
  IF NOT p_lgnum IS INITIAL.
    SELECT COUNT(*)
      FROM t300
      WHERE lgnum EQ p_lgnum.
    IF sy-subrc NE 0.
      gv_erro = abap_true.
      MESSAGE 'N° de depósito inválido!' TYPE 'W' DISPLAY LIKE 'E'.
      CLEAR: p_lgnum.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_lgtyp.
  IF NOT p_lgtyp IS INITIAL AND
     NOT p_lgnum IS INITIAL.
    SELECT COUNT(*)
      FROM t301
      WHERE lgnum EQ p_lgnum AND
            lgtyp EQ p_lgtyp.
    IF sy-subrc NE 0.
      gv_erro = abap_true.
      MESSAGE 'Tipo de depósito inválido!' TYPE 'W' DISPLAY LIKE 'E'.
      CLEAR: p_lgtyp.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_dtcont.
  IF NOT p_dtcont IS INITIAL.
    IF p_dtcont LT sy-datum.
      gv_erro = abap_true.
      MESSAGE 'Data informada situa-se no passado!' TYPE 'W' DISPLAY LIKE 'E'.
      CLEAR: p_dtcont.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_qtdpos.
  IF NOT p_dtcont IS INITIAL AND NOT p_lgnum IS INITIAL AND NOT p_lgtyp IS INITIAL.
    SELECT SINGLE minps
      FROM t340d
      INTO lv_minps
      WHERE lgnum EQ p_lgnum.
    IF p_qtdpos GT lv_minps.
      gv_erro = abap_true.
      CONCATENATE 'Qtd. ultrapassa o limite de posições por Documento. Limite' lv_minps INTO DATA(lv_string) SEPARATED BY space.
      MESSAGE lv_string TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR: p_qtdpos.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM limpa_dados.
    WHEN OTHERS.
  ENDCASE.

  CHECK p_lgnum  IS NOT INITIAL AND p_lgtyp IS NOT INITIAL AND
          p_dtcont IS NOT INITIAL AND p_qtdpos IS NOT INITIAL.

*** INPUT LGBER - IF LGNUM EQ 100 OR 110
  IF ( p_lgnum EQ '100' OR p_lgnum EQ '110') AND p_lgber EQ space.
       REFRESH: it_t302t[].

       SELECT lgnum lgtyp lgber lbert FROM t302t
         INTO TABLE it_t302t
         WHERE LGNUM EQ p_lgnum AND
               LGTYP EQ p_lgtyp AND
               SPRAS EQ sy-langu.

       IF sy-subrc NE 0.
            gv_erro = abap_true.
            CONCATENATE 'Tipo de depósito ' p_lgnum '/' p_lgtyp
                        ' não possui área de armz. cadastrada!' INTO DATA(lv_message).
            MESSAGE lv_message TYPE 'W' DISPLAY LIKE 'I'.
            CLEAR: gv_lgber, p_lgber, lv_message.
            EXIT.
       ELSE.
            CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                window_title  = 'Informar a Área de Armazenagem'
                retfield      = 'LGBER'
                dynpprog      = sy-repid
                dynpnr        = sy-dynnr
                value_org     = 'S'
              TABLES
                value_tab  = it_t302t
                return_tab = it_return_tab.

            IF sy-subrc EQ 0 AND it_return_tab IS NOT INITIAL.
              READ TABLE it_return_tab INTO wa_return INDEX 1.
                IF sy-subrc EQ 0.
                  MOVE wa_return-fieldval TO p_lgber.
                ENDIF.
            ELSE.
              gv_erro = abap_true.
              EXIT.
            ENDIF.
       ENDIF.
  ENDIF.

* INICIO DO PROGRAMA
START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM processa_dados.

END-OF-SELECTION.
  PERFORM limpa_dados.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  CHECK gv_erro EQ abap_false.
  gv_ano = sy-datum(4).

  IF p_lgnum  IS NOT INITIAL AND p_lgtyp IS NOT INITIAL AND
     p_dtcont IS NOT INITIAL AND p_qtdpos IS NOT INITIAL.

      IF p_lgber NE space.
          SELECT lgnum lgtyp lgpla lgber skzsi ivivo
            FROM lagp
            INTO TABLE gt_lagp
            WHERE lgnum EQ p_lgnum AND
                  lgtyp EQ p_lgtyp AND
                  lgber EQ p_lgber.
      ELSE.
          SELECT lgnum lgtyp lgpla lgber skzsi ivivo
            FROM lagp
            INTO TABLE gt_lagp
            WHERE lgnum EQ p_lgnum AND
                  lgtyp EQ p_lgtyp.
      ENDIF.

      IF sy-subrc NE 0.
        gv_erro = abap_true.
        MESSAGE 'Não foram encontradas posições para esta seleção!' TYPE 'S' DISPLAY LIKE 'E'.
        SUBMIT z_doc_inv_ramdom_positions WITH p_lgber = space VIA SELECTION-SCREEN.
      ENDIF.

      CHECK gt_lagp IS NOT INITIAL.
      DESCRIBE TABLE gt_lagp LINES gv_ttpos.
*     SELECIONA POSIÇÕES JÁ INVENTARIADAS NA TABELA Z.
            SELECT lgnum lgtyp lgpla lgber ano uname dtcriacao hrcriacao docinv referencia
              FROM ZTAB001
              INTO TABLE gt_ZTAB001
              FOR ALL ENTRIES IN gt_lagp
              WHERE lgnum EQ gt_lagp-lgnum AND
                    lgtyp EQ gt_lagp-lgtyp AND
                    lgpla EQ gt_lagp-lgpla AND
                    lgber EQ gt_lagp-lgber AND
                    ano   EQ gv_ano.
            IF sy-subrc EQ 0.
              DESCRIBE TABLE gt_ZTAB001 LINES gv_posger.
              gv_realiz = ( gv_posger / gv_ttpos ) * 100.
              SPLIT gv_realiz AT '.' INTO DATA(lv_int) DATA(lv_dec).
              CONDENSE: lv_int, lv_dec.
              IF NOT lv_dec IS INITIAL.
                lv_dec = lv_dec(2).
                CONCATENATE lv_int ',' lv_dec INTO gv_realiz.
              ELSE.
                gv_realiz = lv_int.
              ENDIF.
            ELSE.
              CLEAR: gv_posger, gv_realiz.
            ENDIF.
  ELSE.
    MESSAGE 'Preencha todos os campos!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM. " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM PROCESSA_DADOS.

  DATA: lv_possbloq TYPE string,
        lt_lqua     TYPE TABLE OF rlqua,
        lv_ran_int  TYPE qfranint,
        ls_lagp     TYPE lagp,
        lv_hndle    TYPE balloghndl,
        lv_title    TYPE balnrext,
        lv_anplb    TYPE rl04i-anplb,
        ls_rlagp    TYPE ty_zlagp,
        lv_seq      TYPE seqn5,
        lv_tot      TYPE seqn5.

  CHECK gv_erro EQ abap_false.
  CHECK gt_lagp IS NOT INITIAL.

  DATA(lv_posdispo) = gv_ttpos - gv_posger.

  IF lv_posdispo EQ 00000.
* PROCESSO CONCLUÍDO
     gv_erro = abap_true.
     IF p_lgber IS INITIAL.
        CONCATENATE: 'Processo Finalizado para N°.Dep.' p_lgnum
                     '| Tp.Dep' p_lgtyp '| Ano' gv_ano '.' INTO DATA(lv_message) SEPARATED BY space.
     ELSE.
       CONCATENATE: 'Processo Finalizado para N°.Dep.' p_lgnum
                    '| Tp.Dep' p_lgtyp '| Área Armz.' p_lgber '| Ano' gv_ano '.' INTO lv_message SEPARATED BY space.
     ENDIF.
     MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'S'.
  ELSE.
*** ENCONTRA QTD. DE POSIÇÕES DISPONÍVEIS (SEM BLOQUEIO)
      gt_possbloq = gt_lagp.
      DELETE gt_possbloq WHERE skzsi EQ abap_true OR ivivo EQ abap_true.
      DESCRIBE TABLE gt_possbloq LINES DATA(lv_lines).
***   VALIDAÇÃO DA [QTD. SEM BLOQUEIO] X [QTD. INFORMADA](USUÁRIO)
      IF lv_lines GT 0 AND lv_lines LT p_qtdpos.
        gv_erro = abap_true.
        MOVE lv_lines TO lv_possbloq.
        CONCATENATE 'Só estão disponíveis' lv_possbloq 'posições sem bloqueio.'
        INTO DATA(lv_texto) SEPARATED BY space.
        MESSAGE lv_texto TYPE 'S' DISPLAY LIKE 'E'.

      ELSEIF lv_lines GE p_qtdpos.

        DATA returncode(1) type c.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja confirmar a criação do documento?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Cancelar'
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                 = returncode.

        IF returncode = 2.
           EXIT.
        ENDIF.

***     GERA NÚMERO DA POSIÇÃO.
        FREE gt_rlagp.
        ls_rlagp-lgnum = p_lgnum.
        ls_rlagp-lgtyp = p_lgtyp.
        lv_seq = gv_posger.
        DO p_qtdpos TIMES.
          PERFORM f_gera_numpos USING lv_lines CHANGING lv_ran_int.
          READ TABLE gt_possbloq INTO DATA(ls_possbloq) INDEX lv_ran_int.
          ls_rlagp-lgpla = ls_possbloq-lgpla.
          ls_rlagp-lgber = ls_possbloq-lgber.
          ADD 1 TO lv_seq.
          CONCATENATE lv_seq '/' gv_ttpos INTO ls_rlagp-ref.
          APPEND ls_rlagp TO gt_rlagp.
        ENDDO.

        MOVE p_qtdpos TO lv_anplb.

***     CRIA DOCUMENTO DE INVENTÁRIO
        CALL FUNCTION 'L_INV_DOC_CREATE'
          EXPORTING
            iv_lgnum      = p_lgnum
            iv_lgtyp      = p_lgtyp
            iv_kzinv      = space
            iv_irnum      = space
            iv_liakt      = space
            iv_uname      = sy-uname
            iv_pdatu      = sy-datum
            iv_anplb      = lv_anplb
            iv_hndle      = lv_hndle
          TABLES
            it_lagp       = gt_rlagp
            it_lqua       = lt_lqua
          EXCEPTIONS
            error_message = 99.

        IF sy-subrc EQ 0.
          PERFORM f_grava_tabz CHANGING gv_erro.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          gv_erro = abap_true.
        ENDIF.

          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             msgid              = sy-msgid
             msgnr              = sy-msgno
             msgv1              = sy-msgv1
             msgv2              = sy-msgv2
             msgv3              = sy-msgv3
             msgv4              = sy-msgv4
           IMPORTING
             message_text_output = lv_texto.

        IF gv_erro EQ abap_true.
          MESSAGE lv_texto TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE lv_texto TYPE 'S' DISPLAY LIKE 'S'.
          FREE MEMORY.
        ENDIF.

      ELSEIF lv_lines EQ 0.
***    EXIBE ALV POSICOES BLOQUEADAS
        FREE gt_alv.
        SORT: gt_lagp      BY lgnum lgtyp lgpla,
              gt_ZTAB001 BY lgnum lgtyp lgpla ano.
        LOOP AT gt_lagp INTO DATA(ls_agp).
          DATA(ls_tabix) = sy-tabix.
          READ TABLE gt_ZTAB001 INTO DATA(ls_ZTAB001) WITH KEY lgpla = ls_agp-lgpla.
          IF sy-subrc EQ 0.
            DELETE gt_lagp INDEX ls_tabix.
          ENDIF.
        ENDLOOP.
        gt_alv = gt_lagp.
        PERFORM f_gera_alv.
      ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_GERA_NUMPOS
*&---------------------------------------------------------------------*
FORM f_gera_numpos USING p_lv_lines
                    CHANGING p_lv_ran_int.

  CALL FUNCTION 'QF05_RANDOM_INTEGER'
    EXPORTING
      ran_int_max   = p_lv_lines
      ran_int_min   = 1
    IMPORTING
      ran_int       = p_lv_ran_int
    EXCEPTIONS
      invalid_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    READ TABLE gt_possbloq INTO DATA(ls_possbloq) INDEX p_lv_ran_int.
    IF sy-subrc EQ 0.
      READ TABLE gt_rlagp INTO DATA(ls_rlagp) WITH KEY lgpla = ls_possbloq-lgpla.
      IF sy-subrc EQ 0 .
        PERFORM f_gera_numpos USING p_lv_lines CHANGING p_lv_ran_int.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_TABZ
*&---------------------------------------------------------------------*
FORM f_grava_tabz CHANGING p_gv_erro.

  DATA: ls_ZTAB001 TYPE ZTAB001,
        lt_ZTAB001 TYPE TABLE OF ZTAB001.

  CHECK gv_erro EQ abap_false.

  LOOP AT gt_rlagp INTO DATA(gs_rlagp).
    ls_ZTAB001-lgnum      = p_lgnum.
    ls_ZTAB001-lgtyp      = p_lgtyp.
    ls_ZTAB001-lgpla      = gs_rlagp-lgpla.
    ls_ZTAB001-lgber      = gs_rlagp-lgber.
    ls_ZTAB001-ano        = gv_ano.
    ls_ZTAB001-uname      = sy-uname.
    ls_ZTAB001-dtcriacao  = sy-datum.
    ls_ZTAB001-hrcriacao  = sy-uzeit.
    ls_ZTAB001-docinv     = sy-msgv1(10).
    ls_ZTAB001-referencia = gs_rlagp-ref.

    APPEND ls_ZTAB001 TO lt_ZTAB001.

  ENDLOOP.

  IF lt_ZTAB001 IS NOT INITIAL.
    MODIFY ZTAB001 FROM TABLE lt_ZTAB001.
    IF sy-subrc NE 0.
      gv_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GERA_ALV
*&---------------------------------------------------------------------*
FORM f_gera_alv.

*** Create Instance
  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = gr_table
    CHANGING
      t_table      = gt_alv.

  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).
  gr_columns = gr_table->get_columns( ). "Otimizar colunas
  gr_columns->set_optimize( abap_true ). "Otimizar colunas
  gr_display = gr_table->get_display_settings( ).
  gr_display->set_list_header( 'Posições pendentes c/ bloqueio de inventario ativo ou planejado' ).
  gr_table->display( ).

* LIMPAR CAMPOS AO VOLTAR
  FREE MEMORY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
FORM LIMPA_DADOS.

  FREE:  gt_lagp, gt_ZTAB001, gt_rlagp, gt_possbloq, gt_alv, it_t302t.

  CLEAR: gs_lagp, gv_erro, gr_table, gr_display, gr_functions, lv_minps,
         gv_lgber, p_lgnum, p_lgtyp,p_lgber, p_dtcont, p_qtdpos, gv_ano,
         gv_ttpos, gv_posger, gv_realiz.

ENDFORM.